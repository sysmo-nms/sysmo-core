% This file is part of "Enms" (http://sourceforge.net/projects/enms/)
% Copyright (C) 2012 <Sébastien Serre sserre.bx@gmail.com>
% 
% Enms is a Network Management System aimed to manage and monitor SNMP
% target, monitor network hosts and services, provide a consistent
% documentation system and tools to help network professionals
% to have a wide perspective of the networks they manage.
% 
% Enms is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
% 
% Enms is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with Enms.  If not, see <http://www.gnu.org/licenses/>.
% @doc
% @end
-module(monitor_probe).
-behaviour(gen_server).
-behaviour(supercast_channel).
-include("include/monitor.hrl").

% start
-export([
    start_link/1
]).

% supercast_channel
-export([
    get_perms/1,
    sync_request/2
]).

% gen_server
-export([
    init/1,
    handle_call/3, 
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

% API
-export([
    triggered_return/2
]).

-record(state, {
    name
}).


-record(ets_state, {
    name,
    permissions,
    target_name,
    inspectors_state,
    loggers_state,
    exec_state,
    exec_mod,
    tref,
    status_from,
    status
}).

start_link(#probe{name=Name} = Probe) ->
    gen_server:start_link({via, supercast_registrar, {?MODULE, Name}},
        ?MODULE, Probe, []).

%%----------------------------------------------------------------------------
%% supercast API
%%----------------------------------------------------------------------------
get_perms(PidName) ->
    #ets_state{permissions=Perm} = monitor_data_master:get_probe_state(PidName),
    Perm.

sync_request(PidName, CState) ->
    gen_server:cast({via, supercast_registrar, PidName}, {sync_request, CState}).


%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------
triggered_return(PidName, CState) ->
    gen_server:cast({via, supercast_registrar, PidName}, {triggered_return, CState}).


%%----------------------------------------------------------------------------
%% GEN_SERVER
%%----------------------------------------------------------------------------
init(Probe) ->
    gen_server:cast(self(), continue_init),
    {ok, Probe}.

handle_cast(continue_init, Probe) ->
    init_random(),
    {ok, ExecInitState}     = init_probe(Probe),
    {ok, InspectInitState}  = monitor_inspector:init_all(Probe),
    {ok, LoggersInitState}  = monitor_logger:init_all(Probe),
    TRef = initiate_start_sequence(Probe#probe.step, random),
    ES = #ets_state{
        name             = Probe#probe.name,
        permissions      = Probe#probe.permissions,
        target_name      = Probe#probe.belong_to,
        inspectors_state = InspectInitState,
        loggers_state    = LoggersInitState,
        exec_state       = ExecInitState,
        exec_mod         = Probe#probe.monitor_probe_mod,
        tref             = TRef,
        status_from      = erlang:now(),
        status           = Probe#probe.status
    },
    monitor_data_master:set_probe_state(ES),
    {noreply, #state{name=Probe#probe.name}};

handle_cast({probe_return, NewProbeState, PR}, S) ->
    ES  = monitor_data_master:get_probe_state(S#state.name),

    % INSPECT
    Probe  = monitor_data_master:get(probe, S#state.name),
    IState = ES#ets_state.inspectors_state,
    {ok, IState2, Probe2} = monitor_inspector:inspect_all(IState, Probe, PR),

    % LOG
    LState = ES#ets_state.loggers_state,
    {ok, Pdus, LState2} = monitor_logger:log_all(LState,PR),
    emit_all(ES#ets_state.name, ES#ets_state.permissions, Pdus),

    % LAUNCH
    TRef = initiate_start_sequence(Probe#probe.step, normal),
    MilliRem  = read_timer(TRef),

    % SEND MESSAGES
    Pdu = monitor_pdu:'PDU-MonitorPDU-fromServer-probeReturn'(
        PR,
        ES#ets_state.target_name,
        ES#ets_state.name,
        MilliRem
    ),
    supercast_channel:emit(?MASTER_CHANNEL, {ES#ets_state.permissions, Pdu}),

    % WRITE
    monitor_data_master:set_probe_state(
        ES#ets_state{
            inspectors_state=IState2,
            loggers_state=LState2,
            tref=TRef,
            exec_state=NewProbeState,
            status_from= erlang:now(),
            status=Probe2#probe.status
        }
    ),

    maybe_write_probe(Probe, Probe2),
    {noreply, S};

handle_cast({sync_request, CState}, S) ->
    ES = monitor_data_master:get_probe_state(S#state.name),
    LS = ES#ets_state.loggers_state,
    {ok, Pdus, LS2} = monitor_logger:dump_all(LS, CState),
    ok  = supercast_channel:subscribe(ES#ets_state.name, CState),
    ok  = supercast_channel:unicast(CState, Pdus),
    monitor_data_master:set_probe_state(ES#ets_state{loggers_state=LS2}),
    {noreply, S};

handle_cast({triggered_return, CState}, S) ->
    ES = monitor_data_master:get_probe_state(S#state.name),

    PartialPR = #probe_return{ 
        status          = ES#ets_state.status,
        original_reply  = "",
        timestamp       = 0,
        key_vals        = []
    },

    MilliRem = read_timer(ES#ets_state.tref),
    Pdu = monitor_pdu:'PDU-MonitorPDU-fromServer-probeReturn'(
        PartialPR,
        ES#ets_state.target_name,
        ES#ets_state.name,
        MilliRem
    ),
    supercast_channel:unicast(CState, [Pdu]),

    {noreply, S};

handle_cast(_Call, S) ->
    {noreply, S}.

handle_call(_Call, _From, S) ->
    {noreply, S}.

handle_info(take_of, S) ->
    ES = monitor_data_master:get_probe_state(S#state.name),
    Mod = ES#ets_state.exec_mod,
    ExS = ES#ets_state.exec_state,
    take_of(self(), Mod, ExS),
    {noreply, S};


handle_info(_, SData) ->
    {noreply, SData}.

terminate(_Reason, _S) ->
    normal.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PROBE LAUNCH %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec initiate_start_sequence(
        Step        :: integer(),
        Mode        :: normal | random | now
    ) -> any().
initiate_start_sequence(Step, random) ->
    Step2   = random:uniform(Step),
    initiate_start_sequence(Step2);
initiate_start_sequence(Step, normal) ->
    initiate_start_sequence(Step);
initiate_start_sequence(_, now) ->
    initiate_start_sequence(0).

initiate_start_sequence(Step) ->
    erlang:send_after(Step * 1000, self(), take_of).

take_of(Parent, Mod, ProbeState) ->
    erlang:spawn(fun() ->
        {ok, ProbeState2, Return}  = Mod:exec(ProbeState),
        gen_server:cast(Parent, {probe_return, ProbeState2, Return})
    end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTILS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
emit_all(_, _, []) -> ok;
emit_all(Name, Perm, [Pdu|T]) ->
    supercast_channel:emit(Name,{Perm, Pdu}),
    emit_all(Name,Perm,T).

init_probe(Probe) ->
    Mod       = Probe#probe.monitor_probe_mod,
    InitState = Mod:init(Probe),
    InitState.

init_random() ->
    random:seed(erlang:now()).

read_timer(TRef) ->
    case erlang:read_timer(TRef) of
        false -> 0;
        Any   -> Any
    end.

maybe_write_probe(#probe{name=_}=Probe, #probe{name=_}=Probe) -> ok;
maybe_write_probe(_,Probe2) ->
    monitor_data_master:update(probe, Probe2).

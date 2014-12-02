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
-behaviour(gen_fsm).
-behaviour(supercast_channel).
-include("include/monitor.hrl").

% start
-export([
    start_link/1
]).

% supercast_channel
-export([
    get_perms/1,
    sync_request/2,
    triggered_return/2
]).

% gen
-export([
    init/1,
    handle_event/3, 
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4,
    'RUNNING'/3
]).

-record(state, {
    target_name,
    name,
    probe,
    tref,
    inspectors_state    = [],
    loggers_state       = [],
    probe_state         = []
}).


-record(probe_state, {
    name,
    target_name,
    inspectors_state,
    loggers_state,
    exec_state
}).

start_link(#probe{name = Name} = Probe) ->
    gen_fsm:start_link({via, supercast_registrar, {?MODULE, Name}},
        ?MODULE, Probe, []).

% supercast_channel behaviour
get_perms(PidName) ->
    gen_fsm:sync_send_all_state_event({via, supercast_registrar, PidName}, get_perms).

% supercast_channel behaviour
sync_request(PidName, CState) ->
    gen_fsm:send_all_state_event({via, supercast_registrar, PidName}, {sync_request, CState}).



% from master channel to update clients timeout
triggered_return(PidName, CState) ->
    gen_fsm:send_all_state_event({via, supercast_registrar, PidName}, {triggered_return, CState}).




init(Probe) ->
    init_random(),
    {ok, ProbeInitState}    = init_probe(Probe),
    {ok, InspectInitState}  = monitor_inspector:init_all(Probe),
    {ok, LoggersInitState}  = monitor_logger:init_all(Probe),
    PState = #probe_state{
        name             = Probe#probe.name,
        target_name      = Probe#probe.belong_to,
        inspectors_state = InspectInitState,
        loggers_state    = LoggersInitState,
        exec_state       = ProbeInitState
    },
    monitor_data:set_probe_state(PState),
    PSState = #state{
        target_name         = Probe#probe.belong_to,
        name                = Probe#probe.name,
        probe               = Probe,
        probe_state         = ProbeInitState ,
        inspectors_state    = InspectInitState,
        loggers_state       = LoggersInitState
    },
    {ok, TRef} = initiate_start_sequence(Probe#probe.step, random),
    {ok, 'RUNNING', PSState#state{tref=TRef}, hibernate}.

'RUNNING'(_Event, SName, SData) ->
    {next_state, SName, SData}.

% GEN_CHANNEL event
handle_event({probe_return, NewProbeState, ProbeReturn}, SName, SData) ->
    % update #state.probe_state
    SData1  = SData#state{probe_state = NewProbeState},

    % INSPECT,
    % update #state.probe
    % update #state.inspectors_state,
    Probe        = SData1#state.probe,
    InspectState = SData1#state.inspectors_state,
    {ok, NewInspectState, ModifiedProbe} = 
        monitor_inspector:inspect_all(InspectState, Probe, ProbeReturn),
    SData2  = SData1#state{probe             = ModifiedProbe},
    SData3  = SData2#state{inspectors_state  = NewInspectState},

    % LOG, update loggers_state,
    % update #state.loggers_state
    LoggersState = SData3#state.loggers_state,
    {ok, Pdus, NewLoggersState} = monitor_logger:log_all(LoggersState,ProbeReturn),
    emit_all(Probe#probe.name, Probe#probe.permissions, Pdus),

    SData4 = SData3#state{loggers_state = NewLoggersState},

    % LAUNCH
    % update #state.tref
    P = SData4#state.probe,
    {ok, {NextMicroStart,_} = TRef} = initiate_start_sequence(P#probe.step, normal),
    SData5 = SData4#state{tref = TRef},

    % NOTIFY
    % maybe notify master
    % if Old#state.properties   != New#sate.properties
    % or
    % if Old#state.status       != New#state.status
    %
    % notify all subscribers of '?MASTER_CHANNEL' anyway
    notify(
        ProbeReturn,
        SData5#state.target_name,
        Probe,
        ModifiedProbe,
        NextMicroStart),

    {next_state, SName, SData5};

handle_event({sync_request, CState}, SName, SData) ->
    Probe   = SData#state.probe,
    Name    = Probe#probe.name,
    LStates = SData#state.loggers_state,
    {ok, Pdus, LState2} = monitor_logger:dump_all(LStates, CState),
    ok  = supercast_channel:unicast(CState, Pdus),
    ok  = supercast_channel:subscribe(Name, CState),
    {next_state, SName, SData#state{loggers_state = LState2}};

% Uniquely send to have client timeout counters in sync using tref()
handle_event({triggered_return, CState}, SName, SData) ->
    Probe   = SData#state.probe,
    TargetName  = SData#state.target_name,
    {NMicro, _} = SData#state.tref,
    Status  = Probe#probe.status,
    PName   = Probe#probe.name,

    PartialPr = #probe_return{ 
        status          = Status,
        original_reply  = "",
        timestamp       = 0,
        key_vals        = []
    },

    Pdu = probe_return({PartialPr, TargetName, PName, NMicro}),
    supercast_channel:unicast(CState, [Pdu]),
    {next_state, SName, SData}.

handle_sync_event(get_perms, _From, SName, SData) ->
    Probe = SData#state.probe,
    Perms = Probe#probe.permissions,
    {reply, Perms, SName, SData}.

handle_info(take_of, SName, SData) ->
    #state{
       probe_state = ProbeState,
       probe = #probe{monitor_probe_mod = Mod}
    } = SData,
    take_of(self(), Mod, ProbeState),
    {next_state, SName, SData};

handle_info(_, SName, SData) ->
    {next_state, SName, SData}.

terminate(_Reason, _SName, _SData) ->
    normal.

code_change(_OldVsn, SName, SData, _Extra) ->
    {ok, SName, SData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PROBE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_probe(Probe) ->
    Mod       = Probe#probe.monitor_probe_mod,
    InitState = Mod:init(Probe),
    InitState.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
    timer:send_after(Step * 1000, take_of).

take_of(Parent, Mod, ProbeState) ->
    erlang:spawn(fun() ->
        {ok, ProbeState2, Return}  = Mod:exec(ProbeState),
        gen_fsm:send_all_state_event(Parent, {probe_return, ProbeState2, Return})
    end).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% monitor_master and supercast_channel NOTIFY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
notify(ProbeReturn, TargetName, _OriginalProbe, NewProbe, NextMicroStart) ->
    ok = notify_subscribers(ProbeReturn, TargetName, NewProbe, NextMicroStart),
    monitor_data:write_probe(NewProbe).
    %ok = notify_master(TargetName, OriginalProbe, NewProbe).

notify_subscribers(ProbeReturn, TargetName, Probe, NextMicroStart) ->
    ProbeName = Probe#probe.name,
    Perms    = Probe#probe.permissions,
    Pdu      = probe_return({ProbeReturn, TargetName, ProbeName, NextMicroStart}),
    supercast_channel:emit(?MASTER_CHANNEL, {Perms, Pdu}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTILS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
emit_all(_, _, []) -> ok;
emit_all(Name, Perm, [Pdu|T]) ->
    supercast_channel:emit(Name,{Perm, Pdu}),
    emit_all(Name,Perm,T).


init_random() ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}).

probe_return({
        #probe_return{ 
            status      = Status,
            original_reply = OriginalReply,
            timestamp   = Timestamp,
            key_vals    = KeyVals
        },
        TargetName, ProbeId, NextReturn}) ->
    {modMonitorPDU,
        {fromServer,
            {probeReturn,
                {'ProbeReturn',
                    TargetName,
                    ProbeId,
                    Status,
                    OriginalReply,
                    Timestamp,
                    make_key_values(KeyVals),
                    NextReturn
                }}}}.

make_key_values(K) ->
    make_key_values(K, []).
make_key_values([], S) ->
    S;
make_key_values([{K,V} | T], S) when is_list(V) ->
    make_key_values(T, [{'Property', K, V} | S]);
make_key_values([{K,V} | T], S) when is_integer(V) ->
    make_key_values(T, [{'Property', K, integer_to_list(V)} | S]);
make_key_values([{K,V} | T], S) when is_float(V) ->
    make_key_values(T, [{'Property', K, float_to_list(V, [{decimals, 10}])} | S]);
make_key_values([{K,V} | T], S) when is_atom(V) ->
    make_key_values(T, [{'Property', K, atom_to_list(V)} | S]).

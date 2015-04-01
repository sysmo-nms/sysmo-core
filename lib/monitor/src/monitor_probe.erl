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


-export([start_link/1]).
% supercast_channel
-export([get_perms/1,sync_request/2]).
% gen_server
-export([init/1,handle_call/3, handle_cast/2,handle_info/2,terminate/2,code_change/3]).
% API
-export([triggered_return/2,shutdown/1,force/1,exec_snmp_walk/1]).

% records
-record(state, {name}).

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
    status}).

-record(varbind, {
    oid,
    type,
    value
}).

-record(table_state, {
    agent,
    oids,
    request_oids,
    method
}).




start_link(#probe{name=Name} = Probe) ->
    gen_server:start_link({via, supercast_registrar, {?MODULE, Name}}, ?MODULE, Probe, []).

%%----------------------------------------------------------------------------
%% GEN_SERVER INIT
%%----------------------------------------------------------------------------
init(Probe) ->
    % to let multiple probes initialize in the same time, init is delayed.
    gen_server:cast(self(), continue_init),
    {ok, Probe}.

init2(Probe) ->
    random:seed(erlang:now()),
    {ok, ExecInitState}     = init_snmp_walk(Probe),
    {ok, LoggersInitState}  = monitor_logger:init_all(Probe),
    TRef = initiate_start_sequence(Probe#probe.step, random),
    ES = #ets_state{
        name             = Probe#probe.name,
        permissions      = Probe#probe.permissions,
        target_name      = Probe#probe.belong_to,
        inspectors_state = none,
        loggers_state    = LoggersInitState,
        exec_state       = ExecInitState,
        exec_mod         = Probe#probe.monitor_probe_mod,
        tref             = TRef,
        status_from      = erlang:now(),
        status           = Probe#probe.status
    },
    monitor_data_master:set_probe_state(ES),
    % BEGIN partial return for clients
    PartialReturn = partial_pr(ES),
    MilliRem = read_timer(ES#ets_state.tref),
    Pdu = monitor_pdu:probeReturn(
        PartialReturn,
        ES#ets_state.target_name,
        ES#ets_state.name,
        MilliRem
    ),
    % END partial return for clients
    supercast_channel:emit(?MASTER_CHANNEL, {ES#ets_state.permissions, Pdu}),
    ok.






%%----------------------------------------------------------------------------
%% supercast channel behaviour API
%%----------------------------------------------------------------------------
get_perms(PidName) ->
    #ets_state{permissions=Perm} = monitor_data_master:get_probe_state(PidName),
    Perm.

sync_request(PidName, CState) ->
    gen_server:cast({via, supercast_registrar, PidName}, {sync_request, CState}).
sync_request2(CState, S) ->
    ES = monitor_data_master:get_probe_state(S#state.name),
    LS = ES#ets_state.loggers_state,
    {ok, Pdus, LS2} = monitor_logger:dump_all(LS, CState),
    ok  = supercast_channel:subscribe(ES#ets_state.name, CState),
    ok  = supercast_channel:unicast(CState, Pdus),
    monitor_data_master:set_probe_state(ES#ets_state{loggers_state=LS2}),
    ok.





%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------
-spec triggered_return(PidName::string(), CState::#client_state{}) -> ok.
% @private
% @doc
% Used by the monitor main channel to initialize clients. This function send
% a Partial Probe return PDU to the specified client, including the next expected
% return time.
% @end
triggered_return(PidName, CState) ->
    gen_server:cast({via, supercast_registrar, PidName}, {triggered_return, CState}).
triggered_return2(CState, S) ->
    ES = monitor_data_master:get_probe_state(S#state.name),

    PartialPR = #probe_return{ 
        status          = ES#ets_state.status,
        reply_string    = "",
        timestamp       = 0,
        key_vals        = []
    },

    MilliRem = read_timer(ES#ets_state.tref),
    Pdu = monitor_pdu:probeReturn(
        PartialPR,
        ES#ets_state.target_name,
        ES#ets_state.name,
        MilliRem
    ),
    supercast_channel:unicast(CState, [Pdu]),
    ok.


-spec shutdown(PidName::string()) -> ok.
% @private
% @doc
% Used by monitor datamaster. It shut down the probe specified wile data master
% delete it from the db.
% @end
shutdown(PidName) ->
    case supercast_registrar:whereis_name(PidName) of
        undefined ->
            ok;
        Pid ->
            gen_server:call(Pid, shut_it_down)
    end.

-spec force(PidName::string()) -> ok.
% @private
% @doc
% Force a probe check as soon as possible. Used mainly from monitor API.
% @end
force(PidName) ->
    case supercast_registrar:whereis_name(PidName) of
        undefined ->
            ok;
        Pid ->
            gen_server:cast(Pid, force)
    end.
force2(S) ->
    ES  = monitor_data_master:get_probe_state(S#state.name),
    case erlang:cancel_timer(ES#ets_state.tref) of
        false -> ok;
        _ ->
            TRef = initiate_start_sequence(undefined, now),
            monitor_data_master:set_probe_state(ES#ets_state{tref=TRef}),
            PartialReturn = partial_pr(ES),
            Pdu = monitor_pdu:probeReturn(
                PartialReturn,
                ES#ets_state.target_name,
                ES#ets_state.name,
                500
            ),
            supercast_channel:emit(?MASTER_CHANNEL, {ES#ets_state.permissions, Pdu})
    end.





%%----------------------------------------------------------------------------
%% INTERNALS
%%----------------------------------------------------------------------------
-spec handle_probe_return(Pr::any(), S::any()) -> ok.
% @private
% @doc
% Called by handle_info(probe_return). This function do not need to be exported.
% @end
handle_probe_return(PR, S) ->
    ES  = monitor_data_master:get_probe_state(S#state.name),

    % INSPECT TODO do use case better than behaviour
    [Probe]  = monitor_data_master:get(probe, S#state.name),
    %IState = ES#ets_state.inspectors_state,
    %{ok, IState2, Probe2} = monitor_inspector:inspect_all(IState, Probe, PR),
    OldStatus = Probe#probe.status,
    NewStatus = PR#probe_return.status,
    case NewStatus of
        OldStatus ->
            monitor_events:notify(S#state.name, OldStatus);
        _ ->
            monitor_events:notify_move(S#state.name, NewStatus),
            NewProbe = Probe#probe{status=NewStatus},
            monitor_data_master:update(probe,NewProbe)
    end,

    % LOGGER TODO do use case better than behaviour
    LState = ES#ets_state.loggers_state,
    {ok, Pdus, LState2} = monitor_logger:log_all(LState,PR),
    emit_all(ES#ets_state.name, ES#ets_state.permissions, Pdus),

    % LAUNCH
    TRef = initiate_start_sequence(Probe#probe.step, normal),
    MilliRem  = read_timer(TRef),

    % SEND MESSAGES
    Pdu = monitor_pdu:probeReturn(
        PR,
        ES#ets_state.target_name,
        ES#ets_state.name,
        MilliRem
    ),
    supercast_channel:emit(?MASTER_CHANNEL, {ES#ets_state.permissions, Pdu}),

    % WRITE
    monitor_data_master:set_probe_state(
        ES#ets_state{
            %inspectors_state=IState2,
            loggers_state=LState2,
            tref=TRef,
            %exec_state=NewProbeState,
            status_from= erlang:now(),
            status=PR#probe_return.status
        }
    ).

handle_take_of(S) ->
    ES = monitor_data_master:get_probe_state(S#state.name),
    ExS = ES#ets_state.exec_state,
    take_of(self(), ExS).

take_of(Parent, ProbeState) ->
    erlang:spawn(fun() ->
        {ok, Return}  = exec_snmp_walk(ProbeState),
        erlang:send(Parent, {probe_return, Return})
    end).





%%----------------------------------------------------------------------------
%% HANDLE_CAST
%%----------------------------------------------------------------------------
handle_cast(continue_init, Probe) ->
    init2(Probe),
    {noreply, #state{name=Probe#probe.name}};

handle_cast({sync_request, CState}, S) ->
    sync_request2(CState, S),
    {noreply, S};

handle_cast({triggered_return, CState}, S) ->
    triggered_return2(CState, S),
    {noreply, S};

handle_cast(force, S) ->
    force2(S),
    {noreply, S};

handle_cast(_Cast, S) ->
    {noreply, S}.

%%----------------------------------------------------------------------------
%% HANDLE_CALL
%%----------------------------------------------------------------------------
handle_call(shut_it_down, _F, #state{name=Name} = S) ->
    supercast_channel:delete(Name),
    {stop, shutdown, ok, S};


handle_call(_Call, _From, S) ->
    {noreply, S}.

%%----------------------------------------------------------------------------
%% HANDLE_INFO
%%----------------------------------------------------------------------------
handle_info({probe_return, PR}, S) ->
    handle_probe_return(PR, S),
    {noreply, S};

handle_info(take_of, S) ->
    handle_take_of(S),
    {noreply, S};

handle_info(_, SData) ->
    {noreply, SData}.

%%----------------------------------------------------------------------------
%% OTHER GEN_SERVER
%%----------------------------------------------------------------------------
terminate(_Reason, _S) ->
    normal.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PROBE LAUNCH %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec initiate_start_sequence(Step::integer(),Mode::normal|random|now) -> any().
initiate_start_sequence(Step, random) ->
    Step2   = random:uniform(Step),
    initiate_start_sequence(Step2);
initiate_start_sequence(Step, normal) ->
    initiate_start_sequence(Step);
initiate_start_sequence(_, now) ->
    initiate_start_sequence(0).

initiate_start_sequence(Step) ->
    erlang:send_after(Step * 1000, self(), take_of).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTILS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
emit_all(_, _, []) -> ok;
emit_all(Name, Perm, [Pdu|T]) ->
    supercast_channel:emit(Name,{Perm, Pdu}),
    emit_all(Name,Perm,T).

read_timer(TRef) ->
    case erlang:read_timer(TRef) of
        false -> 0;
        Any   -> Any
    end.

partial_pr(ES) ->
    #probe_return{ 
        status          = ES#ets_state.status,
        reply_string    = "",
        timestamp       = 0,
        key_vals        = []
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SNMP TABLE INIT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_snmp_walk(Probe) ->
    AgentName   = Probe#probe.belong_to,
    Conf        = Probe#probe.monitor_probe_conf,
    Method      = Conf#snmp_probe_conf.method,
    Oids        = Conf#snmp_probe_conf.oids,
    {ok,
        #table_state{
            agent           = AgentName,
            oids            = Oids,
            request_oids    = [Oid || {_, Oid} <- Oids],
            method          = Method
        }
    }.

exec_snmp_walk(#table_state{method = get} = State) ->

    Agent           = State#table_state.agent,
    Request         = State#table_state.request_oids,
    Oids            = State#table_state.oids,

    {_, MicroSec1}  = sys_timestamp(),
    Reply = snmpman:get(Agent, Request),
    {_, MicroSec2}  = sys_timestamp(),

    case Reply of
        {error, _Error} = R ->
            error_logger:info_msg("snmp fail ~p ~p ~p for agent ~p", [?MODULE, ?LINE, R, Agent]),
            KV = [{"status","CRITICAL"},{"sys_latency",MicroSec2 - MicroSec1}],
            OR = to_string(R),
            S  = "CRITICAL",
            PR = #probe_return{
                status          = S,
                reply_string    = OR,
                key_vals        = KV,
                timestamp       = MicroSec2},
            {ok, PR};
        {ok, SnmpReply} ->
            PR  = eval_snmp_get_return(SnmpReply, Oids),
            KV  = PR#probe_return.key_vals,
            KV2 = [{"sys_latency", MicroSec2 - MicroSec1} | KV],
            PR2 = PR#probe_return{
                timestamp = MicroSec2,
                key_vals  = KV2},
            {ok, PR2}
    end;

exec_snmp_walk(#table_state{method=walk_table, oids=Table} = State) ->

    Agent           = State#table_state.agent,

    {_, MicroSec1}  = sys_timestamp(),
    Reply = snmpman:walk_table(Agent, Table),
    {ReplyT, MicroSec2}  = sys_timestamp(),

    case Reply of
        {error, _Error} = R ->
            error_logger:info_msg("snmp fail ~p ~p ~p for agent ~p", [?MODULE, ?LINE, R, Agent]),
            KV = [{"status","CRITICAL"},{"sys_latency",MicroSec2 - MicroSec1}],
            OR = to_string(R),
            S  = "CRITICAL",
            PR = #probe_return{
                status          = S,
                reply_string    = OR,
                reply_tuple     = ignore,
                key_vals        = KV,
                timestamp       = ReplyT},
            {ok, PR};
        {ok, {table, SnmpReply}} ->
            KV  = [{"status","OK"},{"sys_latency", MicroSec2 - MicroSec1}],
            PR = #probe_return{
                timestamp       = ReplyT,
                reply_tuple     = SnmpReply,
                status          = "OK",
                key_vals        = KV,
                reply_string    = to_string(SnmpReply)
            },
            {ok, PR}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTILS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @private
eval_snmp_get_return({varbinds, VarBinds}, Oids) ->
    eval_snmp_return(VarBinds, Oids).

%eval_snmp_walk_return(VarBinds, Oids) ->
    %OidsN = [{K, lists:droplast(O)} || {K, O} <- Oids],
    %eval_snmp_return(VarBinds, OidsN).

eval_snmp_return(VarBinds, Oids) ->
    KeyVals = [
        {Key, (lists:keyfind(Oid, 2, VarBinds))#varbind.value} || 
        {Key, Oid} <- Oids
    ],
    #probe_return{
        status          = "OK",
        reply_string    = to_string(VarBinds),
        key_vals        = [{"status", "OK"} | KeyVals]
    }.

to_string(Term) ->
    lists:flatten(io_lib:format("~p", [Term])).

sys_timestamp() ->
    {Meg, Sec, Micro} = os:timestamp(),
    Seconds      = Meg      * 1000000 + Sec,
    Microseconds = Seconds  * 1000000 + Micro,
    {Seconds, Microseconds}.

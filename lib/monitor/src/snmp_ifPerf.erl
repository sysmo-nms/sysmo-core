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
-module(snmp_ifPerf).
-behaviour(gen_server).
-behaviour(supercast_channel).
-include("include/monitor.hrl").
-include("include/monitor_snmp.hrl").

-define(IF_PERF_OIDS, [
    ?IF_INDEX,
    ?IF_IN_OCTETS,
    ?IF_IN_UCASTPKTS,
    ?IF_IN_NUCASTPKTS,
    ?IF_IN_ERRORS,
    ?IF_OUT_OCTETS,
    ?IF_OUT_UCASTPKTS,
    ?IF_OUT_NUCASTPKTS,
    ?IF_OUT_ERRORS]).
-record(table_row, {
    index,
    in_octets,
    in_ucastpkts,
    in_nucastpkts,
    in_errors,
    out_octets,
    out_ucastpkts,
    out_nucastpkts,
    out_errors}).

-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).
-export([get_perms/1, sync_request/2]).
-export([exec_snmp_walk/1]).

-record(state, {name,ref}).
-record(iftable_state, {
    indexes
}).


start_link(#probe{name=Name} = Probe) ->
    gen_server:start_link({via, supercast_registrar, {?MODULE, Name}}, ?MODULE, Probe, []).

do_init(Probe, Ref) ->
    {ok, _DumpDir} = application:get_env(supercast, http_sync_dir),
    IToF = rrd4j_init(Probe),
    random:seed(erlang:now()),
    TRef = monitor:send_after_rand(Probe#probe.step, {take_of, Ref}),
    ES = #ets_state{
        name                = Probe#probe.name,
        permissions         = Probe#probe.permissions,
        belong_to           = Probe#probe.belong_to,
        tref                = TRef,
        current_status_from = erlang:now(),
        current_status      = Probe#probe.status,
        local_state         = #iftable_state{indexes=IToF}
    },
    monitor_data_master:set_probe_state(ES),
    % BEGIN partial return for clients
    PartialReturn = #probe_return{status=ES#ets_state.current_status},
    MilliRem = monitor:read_timer(ES#ets_state.tref),
    Pdu = monitor_pdu:probeReturn(
        PartialReturn,
        ES#ets_state.belong_to,
        ES#ets_state.name,
        MilliRem
    ),
    % END partial return for clients
    supercast_channel:emit(?MASTER_CHANNEL, {ES#ets_state.permissions, Pdu}).





%%----------------------------------------------------------------------------
%% supercast channel behaviour API
%%----------------------------------------------------------------------------
get_perms(PidName) ->
    #ets_state{permissions=Perm} = monitor_data_master:get_probe_state(PidName),
    Perm.

sync_request(PidName, CState) ->
    gen_server:cast({via, supercast_registrar, PidName}, {sync_request, CState}).

do_sync_request(CState, S) ->
    ES = monitor_data_master:get_probe_state(S#state.name),
    % TODO sync
    Pdus = [],
    supercast_channel:subscribe(ES#ets_state.name, CState),
    supercast_channel:unicast(CState, Pdus).
%%----------------------------------------------------------------------------
%% supercast channel behaviour API END
%%----------------------------------------------------------------------------





%%----------------------------------------------------------------------------
%% monitor API
%%----------------------------------------------------------------------------
do_triggered_return(CState, S) ->
    ES = monitor_data_master:get_probe_state(S#state.name),

    PartialPR = #probe_return{status=ES#ets_state.current_status},

    MilliRem = monitor:read_timer(ES#ets_state.tref),
    Pdu = monitor_pdu:probeReturn(
        PartialPR,
        ES#ets_state.belong_to,
        ES#ets_state.name,
        MilliRem
    ),
    supercast_channel:unicast(CState, [Pdu]),
    ok.

do_force(#state{name=PName,ref=Ref}) ->
    ES  = monitor_data_master:get_probe_state(PName),
    case erlang:cancel_timer(ES#ets_state.tref) of
        false -> ok;
        _ ->
            TRef = monitor:send_after(0, {take_of,Ref}),
            monitor_data_master:set_probe_state(ES#ets_state{tref=TRef}),
            PartialReturn = #probe_return{status=ES#ets_state.current_status},
            Pdu = monitor_pdu:probeReturn(
                PartialReturn,
                ES#ets_state.belong_to,
                ES#ets_state.name,
                500
            ),
            supercast_channel:emit(?MASTER_CHANNEL, {ES#ets_state.permissions, Pdu})
    end.
%%----------------------------------------------------------------------------
%% monitor API END
%%----------------------------------------------------------------------------





%%----------------------------------------------------------------------------
%% INTERNALS
%%----------------------------------------------------------------------------
do_handle_probe_return(PR, #state{name=PName,ref=Ref}) ->
    ES  = monitor_data_master:get_probe_state(PName),
    [Probe]  = monitor_data_master:get(probe,PName),
    OldStatus = Probe#probe.status,
    NewStatus = PR#probe_return.status,
    case NewStatus of
        OldStatus ->
            monitor_events:notify(PName, OldStatus);
        _ ->
            monitor_events:notify_move(PName, NewStatus),
            NewProbe = Probe#probe{status=NewStatus},
            monitor_data_master:update(probe,NewProbe)
    end,

    % log rrd
    IToF = ES#ets_state.local_state#iftable_state.indexes,
    Walk = PR#probe_return.reply_tuple,
    Ts   = PR#probe_return.timestamp,
    _RrdUpdatePdu = rrd4j_log(Ts, IToF, Walk),
    % TODO emit
    %emit_all(ES#ets_state.name, ES#ets_state.permissions, [RrdUpdatePdu]),
    
    % TODO check return values and warn or crit

    % LAUNCH
    TRef     = monitor:send_after(Probe#probe.step, {take_of, Ref}),
    MilliRem = monitor:read_timer(TRef),

    % SEND MESSAGES
    Pdu = monitor_pdu:probeReturn(
        PR,
        ES#ets_state.belong_to,
        ES#ets_state.name,
        MilliRem
    ),
    supercast_channel:emit(?MASTER_CHANNEL, {ES#ets_state.permissions, Pdu}),

    % WRITE
    monitor_data_master:set_probe_state(
        ES#ets_state{
            tref=TRef,
            current_status_from= erlang:now(),
            current_status=PR#probe_return.status
        }
    ).

do_take_of(#state{name=PName,ref=Ref}) ->
    ES      = monitor_data_master:get_probe_state(PName),
    Agent   = ES#ets_state.belong_to,
    ToPid   = self(),
    erlang:spawn(fun() ->
        {ok, Return}  = ?MODULE:exec_snmp_walk(Agent),
        erlang:send(ToPid, {probe_return, Ref, Return})
    end).
%%----------------------------------------------------------------------------
%% INTERNALS END
%%----------------------------------------------------------------------------






%%----------------------------------------------------------------------------
%% GEN_SERVER
%%----------------------------------------------------------------------------
init(Probe) ->
    % to let multiple probes initialize in the same time, init is delayed.
    gen_server:cast(self(), do_init),
    {ok, Probe}.


handle_cast(do_init, #probe{name=PName} = Probe) ->
    Ref = make_ref(),
    do_init(Probe, Ref),
    {noreply, #state{name=PName,ref=Ref}};

handle_cast({sync_request, CState}, S) ->
    do_sync_request(CState, S),
    {noreply, S};

handle_cast({triggered_return, CState}, S) ->
    do_triggered_return(CState, S),
    {noreply, S};

handle_cast(force, S) ->
    do_force(S),
    {noreply, S};

handle_cast(_Cast, S) ->
    {noreply, S}.


handle_call(shut_it_down, _F, #state{name=Name} = S) ->
    supercast_channel:delete(Name),
    {stop, shutdown, ok, S};

handle_call(_Call, _From, S) ->
    {noreply, S}.


handle_info({probe_return, Ref, PR}, #state{ref=Ref} = S) ->
    do_handle_probe_return(PR, S),
    {noreply, S};

handle_info({take_of, Ref}, #state{ref=Ref} = S) ->
    do_take_of(S),
    {noreply, S};

handle_info(_, SData) ->
    {noreply, SData}.


terminate(_Reason, _S) ->
    normal.


code_change(_OldVsn, S, _Extra) ->
    {ok, S}.


%%----------------------------------------------------------------------------
%% UTILS
%%----------------------------------------------------------------------------
%emit_all(_, _, []) -> ok;
%emit_all(Name, Perm, [Pdu|T]) ->
%    supercast_channel:emit(Name,{Perm, Pdu}),
%    emit_all(Name,Perm,T).

%%----------------------------------------------------------------------------
%% SNMP TABLE INIT
%%----------------------------------------------------------------------------
exec_snmp_walk(Agent) ->
    Reply       = snmpman:walk_table(Agent, ?IF_PERF_OIDS),
    {ReplyT, _} = monitor:timestamp(),

    case Reply of
        {error, _Error} = R ->
            error_logger:info_msg("snmp fail ~p ~p ~p for agent ~p", [?MODULE, ?LINE, R, Agent]),
            OR = lists:flatten(io_lib:format("~p",[R])),
            S  = "CRITICAL",
            PR = #probe_return{
                timestamp       = ReplyT,
                status          = S,
                reply_tuple     = ignore,
                reply_string    = OR},
            {ok, PR};
        {ok, {table, SnmpReply}} ->
            PR = #probe_return{
                timestamp       = ReplyT,
                status          = "OK",
                reply_tuple     = SnmpReply,
                reply_string    = "Snmpwalk OK: ifPerfs Walk Success"
            },
            {ok, PR}
    end.

%%----------------------------------------------------------------------------
%% errd4j functions
%%----------------------------------------------------------------------------
rrd4j_init(#probe{name=PName,step=Step,belong_to=TargetName,module_config=Indexes}) ->

    % get the target directory TargetDir
    [Target]    = monitor_data_master:get(target, TargetName),
    TargetDir   = proplists:get_value(var_directory, Target#target.sys_properties),

    % generate the probe directory
    ProbeDirPath = filename:join([TargetDir, PName]),
    case filelib:is_dir(ProbeDirPath) of
        true  -> ok;
        false ->
            ok = file:make_dir(ProbeDirPath)
    end,


    % generate indexes to rrdfiles bind
    IndexesToFiles = [
        {
            Index,
            filename:join([ProbeDirPath, lists:concat(["ifPerfIndex",Index,".rrd"])])
        } || Index <- Indexes],

    % test if files exist and keep missing index files
    MissingFiles = lists:filter(fun({_,File}) ->
        case filelib:is_regular(File) of
            true -> false;
            false -> true
        end
    end, IndexesToFiles),

    % create DS definition for missing files if non empty
    case MissingFiles of
        [] -> ok;
        _ ->
            HeartBeat = Step * 2,
            MCreate = [
                {
                    File,
                    Step,
                    make_ds(HeartBeat),
                    "default"
                } || {_,File} <- MissingFiles],
            ok = errd4j:multi_create(MCreate)
    end,

    % return indexes to files
    IndexesToFiles.

rrd4j_log(Ts, IToF, Walk) ->
    Updates = make_up(Ts,IToF,Walk),
    Ret = errd4j:multi_update(Updates),
    ?LOG({ret,      Ret}),
    ?LOG({up,       Updates}),
    ?LOG({idx,      IToF}),
    ?LOG({walk,     Walk}).

make_up(Ts, IToF, Walk) -> make_up(Ts,IToF,Walk,[]).
make_up(_ , []  , _   ,Up) -> Up;
make_up(Ts,[{Index,File}|Rest],Walk, Up) ->
    % TODO test
    % lists:keytake reduce the size of the list at each iteration, but it 
    % also create a new list. Does it really increase performances?
    % Use keysort and takewhile?
    case lists:keytake(Index, 2, Walk) of
        {value, Row, FWalk} ->
            Updates = [
                {"OctetsIn",            Row#table_row.in_octets},
                {"UnicastPacketsIn",    Row#table_row.in_ucastpkts},
                {"NonUnicastPacketsIn", Row#table_row.in_nucastpkts},
                {"ErrorsIn",            Row#table_row.in_errors},

                {"OctetsOut",           Row#table_row.out_octets},
                {"UnicastPacketsOut",   Row#table_row.out_ucastpkts},
                {"NonUnicastPacketsOut",Row#table_row.out_nucastpkts},
                {"ErrorsOut",           Row#table_row.out_errors}
            ],
            UpCom = {File, Updates, Ts},
            make_up(Ts,Rest,FWalk,[UpCom|Up]);
        false ->
            make_up(Ts,Rest,Walk,Up)
    end.
    
make_ds(HeartBeat) ->
    [
        {"OctetsIn",                "COUNTER", HeartBeat, 0, 'Nan'},
        {"OctetsOut",               "COUNTER", HeartBeat, 0, 'Nan'},
        {"UnicastPacketsIn",        "COUNTER", HeartBeat, 0, 'Nan'},
        {"UnicastPacketsOut",       "COUNTER", HeartBeat, 0, 'Nan'},
        {"NonUnicastPacketsIn",     "COUNTER", HeartBeat, 0, 'Nan'},
        {"NonUnicastPacketsOut",    "COUNTER", HeartBeat, 0, 'Nan'},
        {"ErrorsIn",                "COUNTER", HeartBeat, 0, 'Nan'},
        {"ErrorsOut",               "COUNTER", HeartBeat, 0, 'Nan'}
    ].

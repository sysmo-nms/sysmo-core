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
% @private
-module(monitor_commander).
-behaviour(supercast_commander).
-behaviour(gen_server).
-include("include/monitor.hrl").
-include("include/monitor_snmp.hrl").
-include_lib("kernel/include/file.hrl").

% API
-export([
    start_link/0,
    handle_command/2
]).

% INTERNAL
% should not be called from outside
-export([
    spawn_exec/2,
    handle_create_probe/4
]).

% GEN_SERVER
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

-record(state, {
    data_dir,
    check_db_ref
}).

-define(DETS_CHECK_INFO, check_infos_db).

% used to create random target and probe names
% 1 000 000 possible values
-define(RAND_RANGE, 1000000).
% but must be a minimum of 100000
-define(RAND_MIN,   99999).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

handle_command(Command, CState) ->
    {modMonitorPDU, {fromClient, CastCommand}} = Command,
    gen_server:cast(?MODULE, {CastCommand, CState}).

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% INIT
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
init([]) -> 
    random:seed(erlang:now()),
    {ok, DataDir}  = application:get_env(monitor, targets_data_dir),
    {ok, DetsRef}  = init_check_info_database(DataDir),
    State = #state{
        data_dir=DataDir,
        check_db_ref=DetsRef
    },
    {ok, State}.

init_check_info_database(VarDir) ->
    DetsFile     = filename:absname_join(VarDir, "check_infos.dets"),
    case filelib:is_file(DetsFile) of
        true  ->
            ok = file:delete(DetsFile);
        false ->
            ok
    end,
    {ok, N} = dets:open_file(?DETS_CHECK_INFO, [
        {file,   DetsFile},
        {keypos, 1},
        {ram_file, false},
        {auto_save, 180000},
        {type, set}
    ]),
    dets:close(N),
    dets:open_file(DetsFile).

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% HANDLE_CALL 
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
handle_call(_R, _F, S) ->
    {noreply, S}.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% HANDLE_CAST
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------

%handle_cast({{createSimpleProbe, _ = Com}, CState}, S) ->
    %CheckDir    = S#state.check_dir,
    %{ok, Name}  = generate_id("probe-"),
    %spawn(?MODULE, handle_create_probe, [Com, CheckDir, CState, Name]),
    %{noreply, S};

handle_cast({{createTarget,
        {_, _, _, _, "undefined", "undefined", "undefined", _} = Command}, 
    CState}, S) ->
    VarDir = S#state.data_dir,
    {ok, ReplyPdu} = handle_create_target(Command, VarDir),
    send(CState, ReplyPdu),
    {noreply, S};

handle_cast({{query, {_, QueryId, "getChecksInfo"}}, CState}, S) ->
    DbRef = S#state.check_db_ref,
    Infos = dets:foldr(fun(X, Acc) ->
        {_,I} = X,
        [I|Acc]
    end, [], DbRef),
    ReplyPdu    = pdu(getCheckReply, {QueryId, true, Infos}),
    send(CState, ReplyPdu),
    {noreply, S};

handle_cast({{query, {_, QueryId, Other}}, CState}, S) ->
    Rep = lists:flatten(io_lib:format("Query ~p not unknown", [Other])),
    send(CState, pdu(monitorReply, {QueryId, false, Rep})),
    error_logger:info_msg(
        "unknown cast for query: ~p from client ~p~n",
        [Other, CState]
    ),
    {noreply, S};

handle_cast({{extendedQueryFromClient, 
        {_, QueryId, {snmpElementInfoQuery, Query}}}, CState}, S) ->

    %Perms = monitor_master:get_perms(
    command_net_element_wizard:handle_snmpElementInfoQuery(QueryId, CState, Query),
    {noreply, S};

handle_cast({{extendedQueryFromClient, 
        {_, QueryId, {snmpElementCreateQuery, Query}}}, CState}, S) ->
    command_net_element_wizard:handle_snmpElementCreateQuery(QueryId, CState, Query, S#state.data_dir),
    {noreply, S};


handle_cast(R, S) ->
    error_logger:info_msg(
        "unknown cast for command ~p ~p ~p~n", [?MODULE, ?LINE, R]
    ),
    {noreply, S}.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% HANDLE_INFO
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
handle_info(_I, S) ->
    {noreply, S}.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% TERMINATE  
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
terminate(_R, #state{check_db_ref=Ref}) ->
    dets:close(Ref),
    normal.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% CODE_CHANGE
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
code_change(_O, S, _E) ->
    {ok, S}.



%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% MONITOR COMMANDS
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
% this function is spawned because we use open_port/2
handle_create_probe(Command, CheckDir, CState, Name) ->
    {_,Target,DisplayName,Descr,Perm,"simple",Timeout,Step,Flags,Exe,QId} = Command,
    {_, R, W}   = Perm,
    ExeF        = filename:join(CheckDir, filename:basename(Exe)),
    Args = [{F,V} || {_,F,V} <- Flags],
    Probe = #probe{
        name        =   Name,
        description =   DisplayName,
        info        =   Descr,
        permissions =   #perm_conf{read=R,write=W},
        monitor_probe_mod   = bmonitor_probe_ncheck,
        monitor_probe_conf  = #ncheck_probe_conf{
            executable  = ExeF,
            args        = Args,
            eval_perfs  = false
        },
        timeout     =   Timeout,
        step        =   Step,
        inspectors  = [
            {inspector, bmonitor_inspector_status_set, []},
            {inspector, bmonitor_inspector_property_set, ["status"]}
        ],
        loggers = []
    },

    % Test if the command is well formed. Only check the return status wich
    % should not be 0.
    ExeArgs = [
        lists:flatten(io_lib:format("--~s=~s", [K,V])) 
        || {K,V} <- Args],
    erlang:open_port({spawn_executable, ExeF}, [
        exit_status,
        stderr_to_stdout,
        {args, ExeArgs}
    ]),
    {ExitStatus, Rep} = get_port_reply(),

    case ExitStatus of
        0 ->
            Reply = monitor_master:create_probe(Target, Probe),
            case Reply of
                {error, Error} ->
                    Pdu = pdu(monitorReply, {QId, false, Error}),
                    send(CState, Pdu);
                ok ->
                    Pdu = pdu(monitorReply, {QId, true, "success"}),
                    send(CState, Pdu)
            end;
        _ ->
            Pdu = pdu(monitorReply, {QId, false, Rep}),
            send(CState, Pdu)
    end.

handle_create_target(Command, VarDir) ->
    {'CreateTarget',
        IpAdd,
        PermConf,
        _,
        _,
        _,
        _,
        QueryId
    } = Command,
    {ok, TargetId}      = monitor_master:generate_id("target-"),
    {ok, PermRecord}    = generate_perm_conf(PermConf),
    {ok, TargetDir}     = generate_target_dir(VarDir, TargetId),
    {ok, Prop}          = generate_properties(Command),
    Target = #target{
        id          = TargetId,
        ip          = IpAdd,
        properties  = Prop,
        global_perm = PermRecord,
        directory   = TargetDir
    },
    monitor_master:create_target(Target),
    {ok, pdu(monitorReply, {QueryId, true, atom_to_list(TargetId)})}.


%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% UTILS
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
generate_perm_conf({_, Read, Write}) ->
    {ok, #perm_conf{read = Read, write = Write}}.

generate_target_dir(VarDir, TargetId) ->
    TargetDir   = filename:join([VarDir, TargetId]),
    AbTargetDir = filename:absname(TargetDir),
    {ok, AbTargetDir}.

generate_properties({_, IpAdd, _, Name, SnmpV2ro, SnmpV2rw, _, _}) ->
    case Name of
        "" -> SName = "undefined";
        _  -> SName = Name
    end,
    {ok, [
        {"ip",          IpAdd},
        {"staticName",  SName},
        {"snmp_ro",     SnmpV2ro},
        {"snmp_rw",     SnmpV2rw},
        {"sysLocation", "undefined"},
        {"sysName",     "undefined"},
        {"dnsName",     "undefined"},
        {"hostname",    "undefined"}
    ]}.
 
spawn_exec({QueryId, CState}, {File, Args}) ->
    erlang:open_port({spawn_executable, File}, [
        exit_status,
        stderr_to_stdout,
        {args, Args}
    ]),
    {_ExitStatus, Rep} = get_port_reply(),
    % TODO ncheck API use xml as check return status. Use exit_status 
    % as a normal executable
    %case ExitStatus of
        %0 ->
        % ok but maybe check as failed
        %_ ->
        % error in the check execution
    %end,
    send(CState, pdu(monitorReply, {QueryId, true, Rep})).

get_port_reply() ->
    get_port_reply("").
get_port_reply(Data) ->
    receive
        {_, {exit_status, S}} ->
            {S, Data};
        {_, {data, NData}} ->
            get_port_reply(lists:append(Data, NData));
        _R ->
            error_logger:info_msg("~p ~p: should not be here ~p~n",
                [?MODULE, ?LINE, _R]
            ),
            {4, Data}
    end.

send(#client_state{module = CMod} = CState, Msg) ->
    CMod:send(CState, Msg).


%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% PDUs
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------

pdu(getCheckReply, {QueryId, Status, Infos}) ->
    {modMonitorPDU,
        {fromServer,
            {getCheckReply,
                {'GetCheckReply',
                    QueryId,
                    Status,
                    Infos }}}};
    
pdu(monitorReply, {QueryId, Status, Info}) ->
    {modMonitorPDU,
        {fromServer,
            {monitorReply,
                {'MonitorReply',
                    QueryId,
                    Status,
                    Info }}}}.

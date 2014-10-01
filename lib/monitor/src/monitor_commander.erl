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
-export([
    start_link/0,
    handle_command/2,
    get_check_infos/1,
    spawn_exec/2,
    handle_create_probe/4
]).

-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

-record(reply, {
    id,
    client_state
}).

-record(state, {
    tpl_dir,
    var_dir,
    check_db_ref,
    replies_waiting = [] :: [#reply{}],
    check_dir
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
    {ok, TplDir}   = application:get_env(monitor, templates_dir),
    {ok, VarDir}   = application:get_env(monitor, targets_data_dir),
    {ok, CheckDir} = application:get_env(monitor, check_dir),
    {ok, DetsRef}  = init_check_info_database(VarDir),
    generate_check_infos(CheckDir, DetsRef),
    State = #state{
        tpl_dir=TplDir,
        var_dir=VarDir,
        check_db_ref=DetsRef,
        check_dir=filename:absname(CheckDir)
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

handle_cast({{createSimpleProbe, _ = Com}, CState}, S) ->
    CheckDir    = S#state.check_dir,
    {ok, Name}  = generate_id("probe-"),
    spawn(?MODULE, handle_create_probe, [Com, CheckDir, CState, Name]),
    {noreply, S};

% this guard catch non snmp targets with no templates
handle_cast({{createTarget,
        {_, _, _, _, "undefined", "undefined", "undefined", _} = Command}, 
    CState}, S) ->
    VarDir = S#state.var_dir,
    {ok, ReplyPdu} = handle_create_target(Command, VarDir),
    send(CState, ReplyPdu),
    {noreply, S};

handle_cast({{createTarget, Command}, CState}, S) ->
    TplDir          = S#state.tpl_dir,
    VarDir          = S#state.var_dir,
    {ok, ReplyPdu}  = handle_create_target(Command, TplDir, VarDir),
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

handle_cast({{simulateCheck, {_, QueryId, Check, Args}}, CState}, S) ->
    Path = filename:join(S#state.check_dir, Check),

    % 1 lauch command
    % 2 fill #state.replies_waiting
    % 3 wait for reply somewere
 
    case filename:pathtype(Path) of
        relative ->
            % do not allow relative paths
            error_logger:info_msg(
            "~p ~p: warning ~p is not a relative path. siulateCheck from ~p~n",
            [?MODULE, ?LINE, Path, CState]),
            % TODO reply error to client and close connexion to mitigate DOS
            {noreply, S};
        _ ->
            PortArgs = [
                lists:flatten(io_lib:format("--~s=~s", [Flag, Val]))
            || {_, Flag, Val} <- Args],
            spawn(?MODULE, spawn_exec, [{QueryId, CState}, {Path, PortArgs}]),
            {noreply, S}
    end;

handle_cast({{extendedQueryMsg, 
        {_, QueryId, {snmpElementInfoQuery, Query}}}, CState}, S) ->
    handle_snmpElementInfoQuery(QueryId, CState, Query),
    {noreply, S};

handle_cast({{extendedQueryMsg, 
        {_, QueryId, {snmpUpdateElementQuery, Query}}}, CState}, S) ->
    handle_snmpUpdateElementQuery(QueryId, CState, Query),
    {noreply, S};


handle_cast(R, S) ->
    error_logger:info_msg(
        "unknown cast for command ~p ~p ~p~n", [?MODULE, ?LINE, R]
    ),
    {noreply, S}.

handle_snmpUpdateElementQuery(_QueryId, _CState, Query) ->
    {ok, Target} = monitor_snmp_utils:generate_standard_snmp_target(Query),
    monitor_master:create_target(Target),
    io:format("update element query ~p~n", [Query]).

handle_snmpElementInfoQuery(QueryId, CState, {
        _,
        {_, IpVer, Ip},
        Port,
        Timeout,
        SnmpVer,
        _Community,
        _SecLevel,
        _SecName,
        _AuthProto,
        _AuthKey,
        _PrivProto,
        _PrivKey} = Args) ->
    
    BeginPdu = pdu(extendedReplyMsgString, {QueryId, true, false, "begin"}),
    send(CState, BeginPdu),
    case SnmpVer of
        "3"  ->
            case snmpman:discovery(Ip, IpVer, Port, Timeout) of
                {ok, EngineId} ->
                    Pdu = pdu(extendedReplyMsgString, {QueryId, true, false, EngineId}),
                    send(CState, Pdu),
                    case monitor_snmp_utils:walk_system(Args, EngineId) of
                        {ok, System} ->
                            Pdu2 = pdu(extendedReplyMsgWalkSystem, {QueryId, true, false, System}),
                            send(CState, Pdu2),
                            case monitor_snmp_utils:walk_ifTable(Args, EngineId) of
                                {ok, Val} ->
                                    Pdu3 = pdu(extendedReplyMsgWalkIfTable, {QueryId, true, true, Val}),
                                    send(CState, Pdu3);
                                {error, Reason} ->
                                    Pdu3 = pdu(extendedReplyMsgString, {QueryId, false, true, Reason}),
                                    send(CState, Pdu3)
                            end;
                        {error, Reason} ->
                            Pdu2 = pdu(extendedReplyMsgString, {QueryId, false, true, Reason}),
                            send(CState, Pdu2)
                        end;
                {error, Reason} ->
                    Pdu = pdu(extendedReplyMsgString, {QueryId, false, true, Reason}),
                    send(CState, Pdu)
            end;
        "2c" -> 
            EngineId = "AAAAAAAAAAAA",
            Pdu = pdu(extendedReplyMsgString, {QueryId, true, false, EngineId}),
            send(CState, Pdu),
            case monitor_snmp_utils:walk_system(Args, EngineId) of
                {ok, System} ->
                    Pdu2 = pdu(extendedReplyMsgWalkSystem, {QueryId, true, false, System}),
                    send(CState, Pdu2),
                    case monitor_snmp_utils:walk_ifTable(Args, EngineId) of
                        {ok, Val} ->
                            Pdu3 = pdu(extendedReplyMsgWalkIfTable, {QueryId, true, true, Val}),
                            send(CState, Pdu3);
                        {error, Reason} ->
                            Pdu3 = pdu(extendedReplyMsgString, {QueryId, false, true, Reason}),
                            send(CState, Pdu3)
                    end;
                {error, Reason} ->
                    Pdu2 = pdu(extendedReplyMsgString, {QueryId, false, true, Reason}),
                    send(CState, Pdu2)
            end;

        "1"  ->
            EngineId = "AAAAAAAAAAAA",
            Pdu = pdu(extendedReplyMsgString, {QueryId, true, false, EngineId}),
            send(CState, Pdu),
            case monitor_snmp_utils:walk_system(Args, EngineId) of
                {ok, System} ->
                    Pdu2 = pdu(extendedReplyMsgWalkSystem, {QueryId, true, false, System}),
                    send(CState, Pdu2),
                    case monitor_snmp_utils:walk_ifTable(Args, EngineId) of
                        {ok, Val} ->
                            Pdu3 = pdu(extendedReplyMsgWalkIfTable, {QueryId, true, true, Val}),
                            send(CState, Pdu3);
                        {error, Reason} ->
                            Pdu3 = pdu(extendedReplyMsgString, {QueryId, false, true, Reason}),
                            send(CState, Pdu3)
                    end;
                {error, Reason} ->
                    Pdu2 = pdu(extendedReplyMsgString, {QueryId, false, true, Reason}),
                    send(CState, Pdu2)
            end;

        _    -> 
            Pdu = pdu(extendedReplyMsgString, {QueryId, false, true, "Unknown SNMP version"}),
            send(CState, Pdu)
    end.



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
%% MONITOR COMMANDS
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
        loggers = [
            {logger, bmonitor_logger_text, []}
        ]
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
    {ok, TargetId}      = generate_id("target-"),
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

handle_create_target(Command, TplDir, VarDir) ->
    {'CreateTarget',
        IpAdd,
        PermConf,
        _,
        _,
        _,
        Template,
        QueryId
    } = Command,
    {ok, TargTemp}      = get_template(TplDir, Template),
    {ok, TargetId}      = generate_id("target-"),
    {ok, PermRecord}    = generate_perm_conf(PermConf),
    {ok, TargetDir}     = generate_target_dir(VarDir, TargetId),
    {ok, Prop}          = generate_properties(Command),

    Target1 = TargTemp#target{
        id          = TargetId,
        ip          = IpAdd,
        properties  = Prop,
        global_perm = PermRecord,
        directory   = TargetDir
    },
    Probes  = [generate_probe(PFun, Target1) || PFun <- Target1#target.probes],
    Target2 = Target1#target{probes = Probes},

    case lists:keyfind(error, 1, Probes) of
        {error, Reason} ->
            RString = lists:flatten(io_lib:format("~p",[Reason])),
            {ok, pdu(monitorReply, {QueryId, false, RString})};
        false ->
            monitor_master:create_target(Target2),
            {ok, pdu(monitorReply, {QueryId, true, "Success"})}
    end.

get_template(TplDir, Template) ->
    File                = filename:flatten([Template, ".tpl.erl"]),
    TplFile             = filename:join([TplDir, File]),
    {ok, [TargTemp]}    = file:consult(TplFile),
    {ok, TargTemp}.


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
    
generate_probe(PFun, Target) ->
    {function, Mod, Fun}    = PFun,
    {ok, ProbeId}           = generate_id("probe-"),

    case erlang:apply(Mod, Fun, [ProbeId, Target]) of
        {ok, PRec}  -> PRec;
        Other       -> {error, Other}
    end.







%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% UTILS
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
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

generate_check_infos(CheckDir, DetsRef) ->
    spawn(?MODULE, get_check_infos, [{CheckDir, DetsRef}]).

get_check_infos({CheckDir, DetsRef}) ->
    {ok, Files} = file:list_dir(CheckDir),
    {ok, Re}    = re:compile("^ncheck_.*"),
    ChecksBin = lists:filter(fun(F) ->
        case re:run(F, Re) of
            {match, _}  -> true;
            nomatch     -> false
        end
    end, Files),
    FilesName   = [filename:join(CheckDir, File) || File <- ChecksBin],
    Infos = get_check_infos(FilesName, []),
    lists:foreach(fun(X) ->
        dets:insert(DetsRef, X)
    end, Infos).
get_check_infos([], Infos) ->
    Infos;
get_check_infos([File|Files], Infos) ->
    erlang:open_port({spawn_executable, File},
        [exit_status, stderr_to_stdout, {args, ["-show-xml-check-def"]}]),
    {_ExitStatus, Data} = get_port_reply(),
    get_check_infos(Files, [{File, Data}|Infos]).

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

%%
generate_id(Head) ->
    Int         = random:uniform(?RAND_RANGE),
    RandId      = Int + ?RAND_MIN,
    RandIdL     = io_lib:format("~p", [RandId]),
    RandIdS     = lists:flatten(RandIdL),
    RandIdF     = lists:concat([Head, RandIdS]),
    ToAtom      = erlang:list_to_atom(RandIdF),
    case monitor_master:id_used(ToAtom) of
        false->  {ok, ToAtom};
        true  -> generate_id(Head)
    end.

send(#client_state{module = CMod} = CState, Msg) ->
    CMod:send(CState, Msg).

build_ifTable([], Acc) ->
    lists:reverse(Acc);
build_ifTable([H|T], Acc) ->
    {table_row, IfIndex, IfDescr, IfType, IfMtu, IfSpeed, IfPhysAddress,
        IfAdminStatus, IfOperStatus, IfLastChange} = H,
    TableRow = {'SnmpInterfaceInfo', IfIndex, IfDescr, IfType, IfMtu,
        IfSpeed, IfPhysAddress, IfAdminStatus, IfOperStatus, IfLastChange},
    build_ifTable(T, [TableRow|Acc]).

pdu(extendedReplyMsgWalkIfTable, {QueryId, Status, Last, Info}) ->
    {table, TableRows} = Info,
    IfTable = build_ifTable(TableRows, []),
    pdu(extendedReplyMsg, {QueryId, Status, Last, {snmpInterfacesInfo, IfTable}});

pdu(extendedReplyMsgWalkSystem, {QueryId, Status, Last, Info}) ->
    {varbinds, Varbinds} = Info,
    {_,_,_,SysDescr}        = lists:keyfind(?SYS_DESCR,         2, Varbinds),
    {_,_,_,SysObjectId}     = lists:keyfind(?SYS_OBJECTID,      2, Varbinds),
    {_,_,_,SysUpTime}       = lists:keyfind(?SYS_UPTIME,        2, Varbinds),
    {_,_,_,SysContact}      = lists:keyfind(?SYS_CONTACT,       2, Varbinds),
    {_,_,_,SysName}         = lists:keyfind(?SYS_NAME,          2, Varbinds),
    {_,_,_,SysLocation}     = lists:keyfind(?SYS_LOCATION,      2, Varbinds),
    {_,_,_,SysServices}     = lists:keyfind(?SYS_SERVICES,      2, Varbinds),
    {_,_,_,SysORLastChange} = lists:keyfind(?SYS_ORLAST_CHANGE, 2, Varbinds),

    InfoTuple = {snmpSystemInfo, {'SnmpSystemInfo', 
                    SysDescr, SysObjectId, SysUpTime, SysContact,
                    SysName, SysLocation, SysServices, SysORLastChange}},
    pdu(extendedReplyMsg, {QueryId, Status, Last, InfoTuple});

pdu(extendedReplyMsgString, {QueryId, Status, Last, InfoAtom}) when is_atom(InfoAtom) ->
    Info = atom_to_list(InfoAtom),
    pdu(extendedReplyMsg, {QueryId, Status, Last, {string, Info}});

pdu(extendedReplyMsgString, {QueryId, Status, Last, Info}) ->
    pdu(extendedReplyMsg, {QueryId, Status, Last, {string, Info}});

pdu(extendedReplyMsg, {QueryId, Status, Last, Info}) ->
    {modMonitorPDU,
        {fromServer,
            {extendedReplyMsg,
                {'ExtendedReplyMsg',
                    QueryId,
                    Status,
                    Last,
                    Info
                }}}};
 
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

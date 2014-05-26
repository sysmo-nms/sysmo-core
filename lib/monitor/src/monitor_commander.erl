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
-include_lib("kernel/include/file.hrl").
-export([
    start_link/0,
    handle_command/2,
    get_check_infos/1
]).

-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

-record(state, {
    tpl_dir,
    var_dir,
    check_db_ref
}).

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
    State = #state{tpl_dir=TplDir, var_dir=VarDir, check_db_ref=DetsRef},
    {ok, State}.

init_check_info_database(VarDir) ->
    DetsFile     = filename:absname_join(VarDir, "check_infos.dets"),
    case filelib:is_file(DetsFile) of
        true  ->
            ok = file:delete(DetsFile);
        false ->
            ok
    end,
    {ok, N} = dets:open_file('targets_db', [
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
handle_cast({{createTarget, Command}, CState}, S) ->
    TplDir          = S#state.tpl_dir,
    VarDir          = S#state.var_dir,
    {ok, ReplyPdu}  = handle_create_target(Command, TplDir, VarDir),
    send(CState, ReplyPdu),
    {noreply, S};

handle_cast({{query, {'Query', QueryId, "getChecksInfo"}}, CState}, S) ->
    DbRef = S#state.check_db_ref,
    Infos = dets:foldr(fun(X, Acc) ->
        {_,I} = X,
        [I|Acc]
    end, [], DbRef),
    ReplyPdu    = pdu(getCheckReply, {QueryId, true, Infos}),
    send(CState, ReplyPdu),
    {noreply, S};

handle_cast({{query, {'Query', QueryId, Other}}, CState}, S) ->
    Rep = lists:flatten(io_lib:format("Query ~p not unknown", [Other])),
    send(CState, pdu(monitorReply, {QueryId, false, Rep})),
    error_logger:info_msg(
        "unknown cast for query: ~p from client ~p~n",
        [Other, CState]
    ),
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
terminate(R, _) ->
    R.

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
handle_create_target(Command, TplDir, VarDir) ->
    {'CreateTarget',
        IpAdd,
        PermConf,
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

generate_properties({_, IpAdd, _, SnmpV2ro, SnmpV2rw, _, _}) ->
    {ok, [
        {"ip",          IpAdd},
        {"snmp_ro",     SnmpV2ro},
        {"snmp_rw",     SnmpV2rw},
        {"sysLocation", "undefined"},
        {"sysName",     "undefined"},
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
    Data = get_check_infos_return(),
    get_check_infos(Files, [{File, Data}|Infos]).
get_check_infos_return() ->
    get_check_infos_return("").
get_check_infos_return(Data) ->
    receive
        {_, {exit_status, _}} ->
            Data;
        {_, {data, NData}} ->
            get_check_infos_return(lists:append(Data, NData));
        _ ->
            Data
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

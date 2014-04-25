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
-export([
    start_link/0,
    handle_command/2
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
    var_dir
}).
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
    {ok, TplDir} = application:get_env(monitor, templates_dir),
    {ok, VarDir} = application:get_env(monitor, targets_data_dir),
    State        = #state{tpl_dir = TplDir, var_dir = VarDir},
    {ok, State}.

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
handle_cast(_R, S) ->
    io:format("unknown cast ~p~n", [_R]),
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
        Snmpv2ro,
        Snmpv2rw,
        Template,
        QueryId
    } = Command,
    File            = filename:flatten([Template, ".tpl"]),
    TplFile         = filename:join([TplDir, File]),
    {ok, [Targ0]}   = file:consult(TplFile),

    {ok, Id}    = generate_id("target-"),
    {_, R, W}   = PermConf,
    PermC       = #perm_conf{read = R, write = W},
    TargetDir   = filename:join([VarDir, Id]),
    AbTargetDir = filename:absname(TargetDir),

    Targ1       = Targ0#target{id           = Id},
    Targ2       = Targ1#target{ip           = IpAdd},
    Prop        = [{ip, IpAdd},{snmp_ro, Snmpv2ro},{snmp_rw, Snmpv2rw}],
    Targ3       = Targ2#target{properties   = Prop},
    Targ4       = Targ3#target{global_perm  = PermC},
    Targ5       = Targ4#target{directory    = AbTargetDir},
    Targ6       = fill_probes(Targ5),

    ?LOG({Targ6}),
    Pdu         = pdu(monitorReply, {QueryId, true, "hello"}),
    {ok, Pdu}.

fill_probes(Target) ->
    Probes      = Target#target.probes,
    NewProbes   = [],
    fill_probes(Target, NewProbes, Probes).
fill_probes(Target, NewProbes, []) ->
    TargetR = Target#target{probes = NewProbes},
    TargetR;
fill_probes(Target, NewProbes, [P|Probes]) ->
    % #probe.name
    {ok, PName} = generate_id("probe-"),
    PName2      = erlang:list_to_atom(PName),
    P0          = P#probe{name = PName2},

    % #probe.permissions
    case P0#probe.permissions of
        'TEMPLATE'          ->
            PermConf    = Target#target.global_perm,
            P1          = P0#probe{permissions = PermConf};
        {perm_conf, _, _}   ->
            P1          = P0
    end,

    % #probe.monitor_probe_conf
    case P1#probe.monitor_probe_mod of
        bmonitor_probe_nagios ->
            {ip, TargetIp} = lists:keyfind(ip, 1, Target#target.properties),
            ProbeConf   = P1#probe.monitor_probe_conf,
            ConfList    = ProbeConf#nagios_plugin_conf.args,
            ConfList2   = lists:keystore("-H", 1, ConfList, {"-H", TargetIp}),
            ProbeConf2  = ProbeConf#nagios_plugin_conf{args = ConfList2},
            P2          = P1#probe{monitor_probe_conf = ProbeConf2};
        bmonitor_probe_snmp   ->
            Prop        = Target#target.properties,
            {snmp_ro, Ro} = lists:keyfind(snmp_ro, 1, Prop),
            ProbeConf   = P1#probe.monitor_probe_conf,
            ProbeConf2  = ProbeConf#snmp_conf{community = Ro},
            P2          = P1#probe{monitor_probe_conf = ProbeConf2}
    end,

    % #probe.loggers TODO, include auto generated rrdconfig
    P3  = P2,

    fill_probes(Target, [P3|NewProbes], Probes).



    

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% UTILS
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
generate_id(Head) ->
    Int         = random:uniform(?RAND_RANGE),
    RandId      = Int + ?RAND_MIN,
    RandIdL     = io_lib:format("~p", [RandId]),
    RandIdS     = lists:flatten(RandIdL),
    RandIdF     = lists:concat([Head, RandIdS]),
    {ok, RandIdF}.

send(#client_state{module = CMod} = CState, Msg) ->
    CMod:send(CState, Msg).

pdu(monitorReply, {QueryId, Status, Info}) ->
    {modMonitorPDU,
        {fromServer,
            {monitorReply,
                {'MonitorReply',
                    QueryId,
                    Status,
                    Info }}}}.






%%----------------------------------------------------------------------------
% handle_command({fromClient, {createProbe, Msg}}, 
%         #client_state{module = _CMod} = _CState) ->
%     io:format("createProbe ~p~n",[Msg]);
% 
% handle_command({fromClient, {createTarget, Msg}}, 
%         #client_state{module = CMod} = CState) ->
%     {'TargetCreate', 
%         Ip, Hostname, 
%         Sysname, 
%         {'PermConf', Read, Write},
%         CmdId
%     } = Msg,
%     ReadPerm = case Read of
%         [] ->
%             ["admin"];
%         _  ->
%             ["admin"| Read]
%     end,
%     WritePerm = case Write of
%         [] ->
%             ["admin"];
%         _ ->
%             ["admin"| Write]
%     end,
%     case inet_parse:address(Ip) of
%         {ok, EIp}   ->
%             launch_target(#target{
%                 id          = monitor_misc:generate_id(),
%                 properties = [
%                     {ip             , EIp},
%                     {hostname       , Hostname},
%                     {sysname        , Sysname},
%                     {global_perm    , 
%                         #perm_conf{
%                             read    = ReadPerm,
%                             write   = WritePerm
%                         }
%                     }  
% 
%                 ]
%             }, CState, CmdId);
%         {error, _}  ->
%             CMod:send(CState, pdu(comResp, {CmdId, "ERROR: Bad ip format"}))
%     end;
% 
% 
% handle_command({fromClient, Other}, _) ->
%     io:format("Unknown command ~p~n", [Other]).
% 
% 
% 
% % UTILS
% launch_target(Target, #client_state{module = CMod} = CState, CmdId) ->
%     case monitor_target_channel_sup:new(Target) of
%         {ok, Pid} ->
%             Info = erlang:process_info(Pid),
%             {registered_name, RegName} = 
%                 lists:keyfind(registered_name, 1, Info),
%             Rep = lists:append("OK: ", atom_to_list(RegName)),
%             io:format("rep is ~p~n", [Rep]),
%             CMod:send(CState, pdu(comResp, {CmdId, Rep}));
%         Other ->
%             Rep = lists:append("ERROR: ", atom_to_list(Other)),
%             CMod:send(CState, pdu(comResp, {CmdId, Rep}))
%     end.


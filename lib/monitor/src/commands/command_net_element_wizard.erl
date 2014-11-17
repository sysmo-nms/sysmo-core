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
-module(command_net_element_wizard).
-include("include/monitor.hrl").
-include("include/monitor_snmp.hrl").
-include("../equartz/include/equartz.hrl").
-include_lib("kernel/include/file.hrl").

-export([
    handle_snmpElementCreateQuery/4,
    handle_snmpElementInfoQuery/3
]).

-define(RRD_ifPerf_file, "snmp_if_perf.ini").

handle_snmpElementCreateQuery(_QueryId, _CState, Query, DataDir) ->
    {ok, Target} = generate_standard_snmp_target(Query, DataDir),
    monitor_master:create_target(Target).

handle_snmpElementInfoQuery(QueryId, CState, Args) ->
    BeginPdu = pdu(extendedQueryFromServerString, {QueryId, true, false, "begin"}),
    send(CState, BeginPdu),
    case walk_system(Args) of
        {ok, System} ->
            Pdu2 = pdu(extendedQueryFromServerWalkSystem, {QueryId, true, false, System}),
            send(CState, Pdu2),
            case walk_ifTable(Args) of
                {ok, Val} ->
                    Pdu3 = pdu(extendedQueryFromServerWalkIfTable, {QueryId, true, true, Val}),
                    send(CState, Pdu3);
                {error, Reason} ->
                    Pdu3 = pdu(extendedQueryFromServerString, {QueryId, false, true, Reason}),
                    send(CState, Pdu3)
            end;
        {error, Reason} ->
            Pdu2 = pdu(extendedQueryFromServerString, {QueryId, false, true, Reason}),
            send(CState, Pdu2)
    end.

generate_probe_id() ->
    generate_probe_id([]).
generate_probe_id(List) ->
    {ok, Id} = monitor_master:generate_id("probe-"),
    case lists:member(Id, List) of
        true ->
            generate_probe_id(List);
        false ->
            Id
    end.

% @doc
% Two probes:
% - SNMP if perfs,
% - SNMP system infos
% @end
generate_standard_snmp_target(Args, DataDir) ->
    {'SnmpElementCreateQuery',
        {'IpInfo', IpVersion, Ip},
        Port,
        Timeout,
        SnmpVer,
        Community,
        V3SecLevel,
        V3User,
        V3AuthAlgo,
        V3AuthKey,
        V3PrivAlgo,
        V3PrivKey,
        IfSelection
    } = Args,
    {ok, TargetId} = monitor_master:generate_id("target-"),

    P = generate_probe_id(),

    {RrdCreate, RrdUpdate, RrdGraphs} = get_rrd_template(),

    TargetDir = filename:join(DataDir, TargetId),
    IfSelectionList = [
        {Index, lists:concat(["index", Index, ".rrd"])} 
        || Index <- IfSelection
                      ],
    {ok,
     #target{
        id          = TargetId,
        global_perm = #perm_conf{read = ["admin"], write = ["admin"]},
        sys_properties = [
            {snmp_port,     Port},
            {snmp_version,  SnmpVer},
            {snmp_seclevel, V3SecLevel},
            {snmp_community,Community},
            {snmp_usm_user, V3User},
            {snmp_authkey,  V3AuthKey},
            {snmp_authproto,V3AuthAlgo},
            {snmp_privkey,  V3PrivKey},
            {snmp_privproto,V3PrivAlgo},
            {snmp_timeout,  Timeout},
            {snmp_retries,  1},
            {var_directory, TargetDir}
        ],
        properties = [
            {"ip",          Ip},
            {"ipVersion",   IpVersion},
            {"staticName",  atom_to_list(TargetId)},
            {"sysLocation", "undefined"},
            {"sysName",     "undefined"},
            {"dnsName",     "undefined"}
        ],
        jobs   = [
            #job{
                name     = lists:concat(["daily3am-monitor_jobs-update_snmp_system_info-", TargetId]),
                trigger  = ?CRON_EVERY20S,
                module   = monitor_jobs,
                function = update_snmp_system_info,
                argument = TargetId,
                info     = "Update the target sysInfo (sysName, sysLocation) of the target every days at 3am",
                permissions = #perm_conf{read = ["admin"], write = ["admin"]}
            },
            #job{
                name     = lists:concat(["daily4am-monitor_jobs-update_snmp_if_aliases-", TargetId]),
                trigger  = ?CRON_DAILY4AM,
                module   = monitor_jobs,
                function = update_snmp_if_aliases,
                argument = TargetId,
                info     = "Update the target interfaces alliases of the target every days at 4am",
                permissions = #perm_conf{read = ["admin"], write = ["admin"]}
            }
        ],
        probes = 
            [
                #probe{
                    name = P,
                    description = "SNMP:Interfaces performances",
                    info = "Get interfaces octets:in/out ucast:in/out nucast:in/out errors:in/out every 5 minutes",
                    permissions = #perm_conf{read = ["admin"], write = ["admin"]},
                    status = 'UNKNOWN',
                    step    = 10,
                    timeout = Timeout,

                    properties = [
                        {"status", "UNKNOWN"}
                    ],
                    forward_properties = ["sysName", "sysLocation"],

                    parents = [],
                    active = true,

                    monitor_probe_mod  = bmonitor_probe_snmp,
                    monitor_probe_conf = #snmp_probe_conf{
                        method      = walk_table,
                        retries     = 1,
                        oids        = [
                            ?IF_INDEX,
                            ?IF_DESCR,
                            ?IF_IN_OCTETS,
                            ?IF_IN_UCASTPKTS,
                            ?IF_IN_NUCASTPKTS,
                            ?IF_IN_ERRORS,
                            ?IF_OUT_OCTETS,
                            ?IF_OUT_UCASTPKTS,
                            ?IF_OUT_NUCASTPKTS,
                            ?IF_OUT_ERRORS
                        ]
                    },

                    inspectors = [
                        #inspector{
                            module = bmonitor_inspector_status_set, 
                            conf = []
                        }
                    ],
                    loggers = [
                        #logger{
                            module = bmonitor_logger_rrd2, 
                            conf = [
                                {type,                  snmp_table},
                                {rrd_create,            RrdCreate},
                                {row_index_to_rrd_file, IfSelectionList},
                                {rrd_update,            RrdUpdate},
                                {rrd_graph,             RrdGraphs},
                                {row_index_pos_to_rrd_template,
                                    % as defined in walk_table method 
                                    % (+ 1 for the atom table_row)
                                    [
                                        4, %?IF_IN_OCTETS
                                        8, %?IF_OUT_OCTETS
                                        5, %?IF_IN_UCASTPKTS
                                        6, %?IF_IN_NUCASTPKTS
                                        7, %?IF_IN_ERRORS
                                        9, %?IF_OUT_UCASTPKTS
                                        10,%?IF_OUT_NUCASTPKTS
                                        11 %?IF_OUT_ERRORS
                                    ]
                                }
                            ]
                        }
                    ]
                }
            ]
        }
    }.

walk_ifTable(Args) ->
    {_,{_,IpVer, Ip}, Port, Timeout, SnmpVer, Community, SecLevel, SecName,
    AuthProto, AuthKey, PrivProto, PrivKey} = Args,

    case snmpman:register_element(?TMP_ELEMENT,
        [
            {ip_address, Ip},
            {ip_version, IpVer},
            {snmp_version, SnmpVer},
            {security_level, SecLevel},
            {port, Port},
            {timeout, Timeout},
            {community, Community},
            {priv_proto, PrivProto},
            {priv_key, PrivKey},
            {auth_proto, AuthProto},
            {auth_key, AuthKey},
            {security_name, SecName}
        ]) of
        ok ->
            case snmpman:walk_table(?TMP_ELEMENT, ?IF_INFO) of
                {ok, Val} ->
                    Ret = Val;
                {error, _} = Err ->
                    Ret = Err
            end,
            snmpman:unregister_element(?TMP_ELEMENT),
            {ok, Ret};
        {error, Error} ->
            {error, Error}
    end.

walk_system(Args) ->
    {_,{_,IpVer, Ip}, Port, Timeout, SnmpVer, Community, SecLevel, SecName,
    AuthProto, AuthKey, PrivProto, PrivKey} = Args,
    case snmpman:register_element(?TMP_ELEMENT,
        [
            {ip_address, Ip},
            {ip_version, IpVer},
            {snmp_version, SnmpVer},
            {security_level, SecLevel},
            {port, Port},
            {timeout, Timeout},
            {community, Community},
            {priv_proto, PrivProto},
            {priv_key, PrivKey},
            {auth_proto, AuthProto},
            {auth_key, AuthKey},
            {security_name, SecName}
        ]) of
        ok ->
            case snmpman:walk_tree(?TMP_ELEMENT, ?MIB2_SYSTEM) of
                {ok, {varbinds, VBs}} ->
                    Ret = {ok, {varbinds, filter_system_info(VBs)}};
                {error, _} = Err ->
                    Ret = Err
            end,
            snmpman:unregister_element(?TMP_ELEMENT),
            Ret;
        {error, Error} ->
            {error, Error}
    end.
 
filter_system_info(Vbs) ->
    filter_system_info(Vbs, []).
filter_system_info([], Acc) ->
    lists:reverse(Acc);
filter_system_info([
    {varbind, "1.3.6.1.2.1.1.9" ++ _, _,_} | _], Acc) ->
    lists:reverse(Acc);
filter_system_info([H|T], Acc) ->
    filter_system_info(T, [H|Acc]).


get_rrd_template() ->
    %?RRD_ifPerf_file
    {ok, R} = application:get_env(monitor, rrd_templates_dir),
    TplFile = filename:join(R, ?RRD_ifPerf_file),
    {ok, IniBin} = file:read_file(TplFile),
    IniStr = erlang:binary_to_list(IniBin),
    {ok, IniVal} = ini:parse_string(IniStr),

    CreatePart = proplists:get_value(create, IniVal),
    CreateComm = proplists:get_value(command, CreatePart),

    UpdatePart = proplists:get_value(update, IniVal),
    UpdateComm = proplists:get_value(command, UpdatePart),

    GraphPart  = proplists:get_value(graph,  IniVal),
    GraphComm  = [Graph || {graph, Graph} <- GraphPart],

    {CreateComm,UpdateComm,GraphComm}.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% PDUs
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
pdu(extendedQueryFromServerWalkIfTable, {QueryId, Status, Last, Info}) ->
    {table, TableRows} = Info,
    IfTable = build_ifTable(TableRows, []),
    pdu(extendedQueryFromServer, {QueryId, Status, Last, {snmpInterfacesInfo, IfTable}});

pdu(extendedQueryFromServerWalkSystem, {QueryId, Status, Last, Info}) ->
    {varbinds, Varbinds} = Info,
    {_,_,_,SysDescr}        = lists:keyfind(?SYS_DESCR,         2, Varbinds),
    {_,_,_,SysObjectId}     = lists:keyfind(?SYS_OBJECTID,      2, Varbinds),
    {_,_,_,SysUpTime}       = lists:keyfind(?SYS_UPTIME,        2, Varbinds),
    {_,_,_,SysContact}      = lists:keyfind(?SYS_CONTACT,       2, Varbinds),
    {_,_,_,SysName}         = lists:keyfind(?SYS_NAME,          2, Varbinds),
    {_,_,_,SysLocation}     = lists:keyfind(?SYS_LOCATION,      2, Varbinds),
    {_,_,_,SysServices}     = lists:keyfind(?SYS_SERVICES,      2, Varbinds),

    case lists:keyfind(?SYS_ORLAST_CHANGE, 2, Varbinds) of
        {_,_,_,SysORLastChangeV} ->
            SysORLastChange = SysORLastChangeV;
        _ ->
            SysORLastChange = "unknown"
    end,

    InfoTuple = {snmpSystemInfo, {'SnmpSystemInfo', 
                    SysDescr, SysObjectId, SysUpTime, SysContact,
                    SysName, SysLocation, SysServices, SysORLastChange}},
    pdu(extendedQueryFromServer, {QueryId, Status, Last, InfoTuple});

pdu(extendedQueryFromServerString, {QueryId, Status, Last, InfoAtom}) when is_atom(InfoAtom) ->
    Info = atom_to_list(InfoAtom),
    pdu(extendedQueryFromServer, {QueryId, Status, Last, {string, Info}});

pdu(extendedQueryFromServerString, {QueryId, Status, Last, Info}) ->
    pdu(extendedQueryFromServer, {QueryId, Status, Last, {string, Info}});

pdu(extendedQueryFromServer, {QueryId, Status, Last, Info}) ->
    {modMonitorPDU,
        {fromServer,
            {extendedQueryFromServer,
                {'ExtendedQueryFromServer',
                    QueryId,
                    Status,
                    Last,
                    Info
                }}}}.
 

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

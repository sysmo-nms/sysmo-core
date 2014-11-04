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
-module(helper_monitor_snmp).
-include("include/monitor.hrl").
-include("include/monitor_snmp.hrl").
-export([
    walk_ifTable/2,
    get_sysName/2,
    walk_system/2,
    generate_standard_snmp_target/2
]).

% four RRAs from 100 hours to 10 years
% RRA: (1*300)  * 1200 = 360000    seconds = 6000    minutes = 100   hours
% RRA: (12*300) * 2400 = 8640000   seconds = 14400   minutes = 2400  hours = 100 days

% RRA: (50*300) * 6400 = 96000000  seconds = 1600000 minutes = 26666 hours = 1111 days = 3 years
% RRA: (118*300)* 8900 = 315060000 seconds = 5251000 minutes = 87516 hours = 3646 days = 10 years
-define(RRD_ifPerf_CREATE,
" --step 300 --no-overwrite DS:octetsIn:COUNTER:650:U:U DS:octetsOut:COUNTER:650:U:U DS:ucastPkIn:COUNTER:650:U:U DS:nucastPkIn:COUNTER:650:U:U DS:errorsIn:COUNTER:650:U:U DS:ucastPkOut:COUNTER:650:U:U DS:nucastPkOut:COUNTER:650:U:U DS:errorsOut:COUNTER:650:U:U RRA:AVERAGE:0.5:1:1200 RRA:AVERAGE:0.5:12:2400"
).
-define(RRD_ifPerf_UPDATE,
"<FILE> --template octetsIn:octetsOut:ucastPkIn:nucastPkIn:errorsIn:ucastPkOut:nucastPkOut:errorsOut N:<OCTETS-IN>:<OCTETS-OUT>:<UCAST-IN>:<NUCAST-IN>:<ERRORS-IN>:<UCAST-OUT>:<NUCAST-OUT>:<ERRORS-OUT>"
).
-define(RRD_ifPerf_UPDATE_tpl,
"--template octetsIn:octetsOut:ucastPkIn:nucastPkIn:errorsIn:ucastPkOut:nucastPkOut:errorsOut"
).
-define(RRD_ifPerf_GRAPH,
    [
"DEF:oIn=<FILE>:octetsIn:AVERAGE DEF:oOut=<FILE>:octetsOut:AVERAGE LINE1:oIn#3465A4 LINE2:oOut#CC0000",
"DEF:errIn=<FILE>:errorsIn:AVERAGE DEF:errOut=<FILE>:errorsOut:AVERAGE LINE1:errIn#3465A4 LINE2:errOut#CC0000",
"DEF:ucastIn=<FILE>:ucastPkIn:AVERAGE DEF:ucastOut=<FILE>:ucastPkOut:AVERAGE DEF:nucastIn=<FILE>:nucastPkIn:AVERAGE DEF:nucastOut=<FILE>:nucastPkOut:AVERAGE LINE1:ucastIn#3465A4 LINE2:ucastOut#CC0000 LINE3:nucastIn#ff0000 LINE4:nucastOut#00ff00"
    ]
).


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
        EngineId,
        IfSelection
    } = Args,
    {ok, TargetId} = monitor_master:generate_id("target-"),
    P1 = generate_probe_id([]),
    P2 = generate_probe_id([P1]),

    TargetDir = filename:join(DataDir, TargetId),
    io:format("generate standard_snmp_target: ~p ~p ~p ~p ~p~n",[Args, TargetId, P1, P2, TargetDir]),
    io:format("generate standard_snmp_target didi: ~p ~p ~n",[IfSelection, V3User]),

    {ok,
    #target{
        id = TargetId,
        ip = Ip,
        ip_version = IpVersion,
        global_perm = #perm_conf{read = ["admin"], write = ["admin"]},
        directory = TargetDir,
        properties = [
            {"ip",          Ip},
            {"ipVersion",   IpVersion},
            {"staticName",  atom_to_list(TargetId)},
            {"sysLocation", "undefined"},
            {"sysName",     "undefined"},
            {"dnsName",     "undefined"}
        ],
        probes = [
            #probe{
               name = P1,
               description = "SNMP:System informations",
               info = "Get sysName and sysLocation OID every 5 minutes",
               permissions = #perm_conf{read = ["admin"], write = ["admin"]},
               status = 'UNKNOWN',
               step    = 5,
               timeout = Timeout,

               properties = [
                             {"status",      "UNKNOWN"},
                             {"sysName",     "undefined"},
                             {"sysLocation", "undefined"}
               ],
               forward_properties = ["sysName", "sysLocation"],

               parents = [],
               active = true,

                
               monitor_probe_mod  = bmonitor_probe_snmp,
               monitor_probe_conf = #snmp_probe_conf{
                    port        = Port,
                    version     = SnmpVer,
                    seclevel    = V3SecLevel,
                    community   = Community,
                    usm_user    = V3User,
                    authkey     = V3AuthKey,
                    authproto   = V3AuthAlgo,
                    privkey     = V3PrivKey,
                    privproto   = V3PrivAlgo,
                    engine_id   = EngineId,
                    method      = get,
                    retries = 1,
                    oids = [
                        {"sysName", "1.3.6.1.2.1.1.5.0"},
                        {"sysLocation", "1.3.6.1.2.1.1.6.0"}
                    ]
               },

               inspectors = [
                            #inspector{
                               module = bmonitor_inspector_status_set, 
                               conf = []
                              },
                            #inspector{
                               module = bmonitor_inspector_property_set, 
                               conf = ["sysName", "sysLocation"]
                              }

                           ],
               loggers = []
            },
            #probe{
               name = P2,
               description = "SNMP:Interfaces performances",
               info = "Get interfaces octets:in/out ucast:in/out nucast:in/out errors:in/out ",
               permissions = #perm_conf{read = ["admin"], write = ["admin"]},
               status = 'UNKNOWN',
               step    = 5,
               timeout = Timeout,

               properties = [
                    {"status", "UNKNOWN"}
               ],
               forward_properties = ["sysName", "sysLocation"],

               parents = [],
               active = true,

                
               monitor_probe_mod  = bmonitor_probe_snmp,
               monitor_probe_conf = #snmp_probe_conf{
                    port        = Port,
                    version     = SnmpVer,
                    seclevel    = V3SecLevel,
                    community   = Community,
                    usm_user    = V3User,
                    authkey     = V3AuthKey,
                    authproto   = V3AuthAlgo,
                    privkey     = V3PrivKey,
                    privproto   = V3PrivAlgo,
                    engine_id   = EngineId,
                    method      = {walk_table,
                        [
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
                        ],
                        [
                            % this set the property of the probe to have
                            % up to date name of interface.
                            % set the return properties as a list
                            %  of {indexN,  IF_DESCR}
                            {
                                "index",         % The head key element
                                2,               % The element appended to head wich form Key
                                3                % the place in oid tuple of the value Val
                            }
                        ]
                    },
                    retries     = 1,
                    oids        = []
               },

               inspectors = [
                            #inspector{
                               module = bmonitor_inspector_status_set, 
                               conf = []
                              }
                           ],
               loggers = 
               [
                    #logger{
                        module = bmonitor_logger_rrd2, 
                        conf = [
                            {type, snmp_table},
                            {rrd_create, ?RRD_ifPerf_CREATE},
                            {row_index_to_rrd_file, 
                                [
                                    {1,"index1.rrd"},
                                    {2,"index2.rrd"},
                                    {3,"index3.rrd"}
                                ]
                            },
                            {rrd_update, ?RRD_ifPerf_UPDATE_tpl},
                            {rrd_graph, ?RRD_ifPerf_GRAPH},
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

walk_ifTable(Args, EngineId) ->
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
            {security_name, SecName},
            {engine_id, EngineId}
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

walk_system(Args, EngineId) ->
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
            {security_name, SecName},
            {engine_id, EngineId}
        ]) of
        ok ->
            %{ok, {varbinds, VBs}} = snmpman:walk_tree(?TMP_ELEMENT, ?MIB2_SYSTEM),
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

get_sysName(Args, EngineId) ->
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
            {security_name, SecName},
            {engine_id, EngineId}
        ]) of
        ok ->
            Oid = string:concat(?SYS_NAME, ".0"),
            Ret = snmpman:get(?TMP_ELEMENT, Oid),
            snmpman:unregister_element(?TMP_ELEMENT),
            case Ret of
                {ok, [{_, [{_,_,_,Value}]}]} ->
                    {ok, Value};
                Other ->
                    Other
            end;
        {error,_} = Err ->
            Err
    end.

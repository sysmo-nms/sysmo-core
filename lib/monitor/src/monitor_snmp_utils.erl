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
-module(monitor_snmp_utils).
-include("include/monitor.hrl").
-include("include/monitor_snmp.hrl").
-export([
    walk_ifTable/2,
    get_sysName/2,
    walk_system/2,
    generate_standard_snmp_target/1
]).

% four RRAs from 100 hours to 10 years
% RRA: (1*300)  * 1200 = 360000    seconds = 6000    minutes = 100   hours
% RRA: (12*300) * 2400 = 8640000   seconds = 14400   minutes = 2400  hours = 100 days

% RRA: (50*300) * 6400 = 96000000  seconds = 1600000 minutes = 26666 hours = 1111 days = 3 years
% RRA: (118*300)* 8900 = 315060000 seconds = 5251000 minutes = 87516 hours = 3646 days = 10 years
-define(RRD_ifPerf_CREATE,
"create <FILE> --step 300 DS:octetsIn:COUNTER:650:U:U DS:octetsOut:COUNTER:650:U:U DS:ucastPkIn:COUNTER:650:U:U DS:nucastPkIn:COUNTER:650:U:U DS:errorsIn:COUNTER:650:U:U DS:ucastPkOut:COUNTER:650:U:U DS:nucastPkOut:COUNTER:650:U:U DS:errorsOut:COUNTER:650:U:U RRA:AVERAGE:0.5:1:1200 RRA:AVERAGE:0.5:12:2400"
).
-define(RRD_ifPerf_UPDATE,
"update <FILE> --template octetsIn:octetsOut:ucastPkIn:nucastPkIn:errorsIn:ucastPkOut:nucastPkOut:errorsOut N:<OCTETS-IN>:<OCTETS-OUT>:<UCAST-IN>:<NUCAST-IN>:<ERRORS-IN>:<UCAST-OUT>:<NUCAST-OUT>:<ERRORS-OUT>"
).
-define(RRD_ifPerf_GRAPH,
    [
"DEF:oIn=<FILE>:octetsIn:AVERAGE DEF:oOut=<FILE>:octetsOut:AVERAGE LINE1:oIn#3465A4 LINE2:oOut#CC0000",
"DEF:errIn=<FILE>:errorsIn:AVERAGE DEF:errOut=<FILE>:errorsOut:AVERAGE LINE1:errIn#3465A4 LINE2:errOut#CC0000",
"DEF:ucastIn=<FILE>:ucastPkIn:AVERAGE DEF:ucastOut=<FILE>:ucastPkOut:AVERAGE DEF:nucastIn:=<FILE>:nucastPkIn:AVERAGE DEF:nucastOut:=<FILE>:nucastPkOut:AVERAGE LINE1:ucastIn#3465A4 LINE2:ucastOut#CC0000 LINE3:nucastIn#ff0000 LINE4:nucastOut#00ff00"
    ]
).



generate_standard_snmp_target(_Args) ->
    {ok,
     #target{
        id = 'jojo17',
        ip = "192.168.0.5",
        ip_version = "v4",
        global_perm = #perm_conf{read = ["admin"], write = ["admin"]},
        directory = "var/monitor/jojo17",
        properties = [
            {"ip",          "192.168.0.5"},
            {"ipVersion",   "v4"},
            {"staticName",  "jojo17"},
            {"sysLocation", "undefined"},
            {"sysName",     "undefined"},
            {"dnsName",     "undefined"}
        ],
        probes = [
            #probe{
               name = 'jojoprobe',
               description = "jojojojojo",
               info = "jojojojojo",
               permissions = #perm_conf{read = ["admin"], write = ["admin"]},
               status = 'UNKNOWN',
               step    = 5,
               timeout = 2000,

               properties = [
                             {"status", "UNKNOWN"},
                             {"sysName", "undefined"},
                             {"sysLocation", "undefined"}
               ],
               forward_properties = ["sysName", "sysLocation"],

               parents = [],
               active = true,

                
               monitor_probe_mod  = bmonitor_probe_snmp,
               monitor_probe_conf = #snmp_probe_conf{
                    port        = 161,
                    version     = "2c",
                    seclevel    = "noAuthNoPriv",
                    community   = "public",
                    usm_user    = "undefined",
                    authkey     = "undefined",
                    authproto   = "SHA",
                    privkey     = "undefined",
                    privproto   = "AES",
                    engine_id   = "AAAAAAAAAAAA",
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
               loggers = 
               [
                    #logger{
                        module = bmonitor_logger_text, 
                        conf = []
                      }
               ]
            },
            #probe{
               name = 'jojoprobe2',
               description = "jujujuju",
               info = "jojojojojo",
               permissions = #perm_conf{read = ["admin"], write = ["admin"]},
               status = 'UNKNOWN',
               step    = 5,
               timeout = 2000,

               properties = [
                    {"status", "UNKNOWN"}
               ],
               forward_properties = ["sysName", "sysLocation"],

               parents = [],
               active = true,

                
               monitor_probe_mod  = bmonitor_probe_snmp,
               monitor_probe_conf = #snmp_probe_conf{
                    port        = 161,
                    version     = "2c",
                    seclevel    = "noAuthNoPriv",
                    community   = "public",
                    usm_user    = "undefined",
                    authkey     = "undefined",
                    authproto   = "SHA",
                    privkey     = "undefined",
                    privproto   = "AES",
                    engine_id   = "AAAAAAAAAAAA",
                    method      = {walk_table,
                        [
                            ?IF_INDEX,
                            ?IF_DESCR,
                            ?IF_IN_OCTETS,
                            ?IF_IN_UCASTPKTS,
                            ?IF_IN_NUCASTPKTS,
                            ?IF_IN_DISCARDS,
                            ?IF_IN_ERRORS,
                            ?IF_OUT_OCTETS,
                            ?IF_OUT_UCASTPKTS,
                            ?IF_OUT_NUCASTPKTS,
                            ?IF_OUT_DISCARDS,
                            ?IF_OUT_ERRORS
                        ],
                        [
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
                            {table_index_to_rrd_file, 
                                [
                                    {1,"index1.rrd"},
                                    {2,"index2.rrd"},
                                    {3,"index3.rrd"}
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
            io:format("ret : ~p~n",[Ret]),
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

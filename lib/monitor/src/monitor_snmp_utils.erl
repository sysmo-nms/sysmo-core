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

generate_standard_snmp_target(_Args) ->
    {ok,
     #target{
        id = 'jojo17',
        ip = "192.168.0.5",
        ip_version = "v4",
        global_perm = #perm_conf{read = ["admin"], write = ["admin"]},
        properties = [
            {"ip", "192.168.0.5"},
            {"staticName", "jojo17"},
            {"sysLocation", "undefined"},
            {"sysName", "undefined"},
            {"dnsName", "undefined"}
        ],
        probes = [
            #probe{
               id = 0,
               name = 'jojoprobe',
               description = "jojojojojo",
               info = "jojojojojo",
               permissions = #perm_conf{read = ["admin"], write = ["admin"]},
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
                    method = get,
                    oids = [
                        {"sysName", "1.3.6.1.2.1.1.5.0"},
                        {"sysLocation", "1.3.6.1.2.1.1.6.0"}
                    ],
                    retries = 1
               },
               status = 'UNKNOWN',
               timeout = 2000,
               step    = 5,
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
               loggers = [
                          #logger{
                             module = bmonitor_logger_text, 
                             conf = []
                            }
                         ],
               properties = [
                             {"status", "UNKNOWN"},
                             {"sysName", "undefined"},
                             {"sysLocation", "undefined"}
                            ],
               forward_properties = ["status", "sysName", "sysLocation"],
               active = true
            }
        ],
        directory = "var/monitor/jojo17"
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

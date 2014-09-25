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
    walk_system/2
]).


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
            _Ret = snmpman:walk_table(?TMP_ELEMENT, ?IF_INFO),
            snmpman:unregister_element(?TMP_ELEMENT),
            io:format("ret is ~p~n",[_Ret]),
            {ok, "if infos"};
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

        {error, Error} ->
            {error, Error}
    end.

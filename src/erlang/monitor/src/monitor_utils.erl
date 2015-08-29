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
-module(monitor_utils).
-include("include/monitor.hrl").
-include("include/monitor_snmp.hrl").

-export([
    init_target_snmp/1,
    init_target_dir/1,
    walk_ifTable/2
]).

init_target_dir(Target) ->
    Dir = proplists:get_value(var_directory, Target#target.sys_properties),
    case file:read_file_info(Dir) of
        {ok, _} ->
            ok;
        {error, enoent} ->
            file:make_dir(Dir);
        Other ->
            exit({error, Other})
    end.


init_target_snmp(Target) ->
    SysProp = Target#target.sys_properties,
    case proplists:get_value("snmp_version", SysProp) of
        undefined   -> ok;
        SnmpVersion ->
            SnmpPort        = proplists:get_value("snmp_port",        SysProp),
            SnmpSecLevel    = proplists:get_value("snmp_seclevel",    SysProp),
            SnmpCommunity   = proplists:get_value("snmp_community",   SysProp),
            SnmpUsmUser     = proplists:get_value("snmp_usm_user",    SysProp),
            SnmpAuthKey     = proplists:get_value("snmp_authkey",     SysProp),
            SnmpAuthProto   = proplists:get_value("snmp_authproto",   SysProp),
            SnmpPrivKey     = proplists:get_value("snmp_privkey",     SysProp),
            SnmpPrivProto   = proplists:get_value("snmp_privproto",   SysProp),
            SnmpTimeout     = proplists:get_value("snmp_timeout",     SysProp),
            SnmpRetries     = proplists:get_value("snmp_retries",     SysProp),

            Props = Target#target.properties,
            Host = proplists:get_value("host", Props),

            SnmpArgs = [
                {host,              Host},
                {timeout,           SnmpTimeout},
                {port,              SnmpPort},
                {snmp_version,      SnmpVersion},
                {security_level,    SnmpSecLevel},
                {community,         SnmpCommunity},
                {auth_key,          SnmpAuthKey},
                {auth_proto,        SnmpAuthProto},
                {priv_key,          SnmpPrivKey},
                {priv_proto,        SnmpPrivProto},
                {retries,           SnmpRetries},
                {security_name,     SnmpUsmUser}
            ],
            snmpman:register_element(Target#target.name, SnmpArgs)
    end.

walk_ifTable(Props,SProps) ->
    PortString      = proplists:get_value("snmp_port",      SProps),
    TimeoutString   = proplists:get_value("snmp_timeout",   SProps),
    SnmpVer         = proplists:get_value("snmp_version",   SProps),
    Community       = proplists:get_value("snmp_community", SProps, "public"),
    SecLevel        = proplists:get_value("snmp_seclevel",  SProps, "noAuthNoPriv"),
    SecName         = proplists:get_value("snmp_usm_user",  SProps, "undefined"),
    AuthProto       = proplists:get_value("snmp_authproto", SProps, "MD5"),
    AuthKey         = proplists:get_value("snmp_authkey",   SProps, "undefined"),
    PrivProto       = proplists:get_value("snmp_privproto", SProps, "DES"),
    PrivKey         = proplists:get_value("snmp_privkey",   SProps, "undefined"),
    Host            = proplists:get_value("host",           Props),
    Port    = erlang:list_to_integer(PortString),
    Timeout = erlang:list_to_integer(TimeoutString),

    case snmpman:register_element(?TMP_AGENT,
        [
            {host,          Host},
            {snmp_version,  SnmpVer},
            {security_level,SecLevel},
            {port,          Port},
            {timeout,       Timeout},
            {community,     Community},
            {priv_proto,    PrivProto},
            {priv_key,      PrivKey},
            {auth_proto,    AuthProto},
            {auth_key,      AuthKey},
            {security_name, SecName}
        ]) of
        ok ->
            R = snmpman:walk_table(?TMP_AGENT, ?IF_INFO),
            snmpman:unregister_element(?TMP_AGENT),
            R;
        {error, Error} ->
            {error, Error}
    end.

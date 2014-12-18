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
-include("include/monitor_snmp.hrl").
-include("include/monitor.hrl").
-behaviour(supercast_commander).
-behaviour(gen_server).

% GEN_SERVER
-export([
    start_link/0,
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

% SUPERCAST_COMMANDER
-export([
    handle_command/2
]).

handle_command(Command, CState) ->
    {modMonitorPDU, {fromClient, CastCommand}} = Command,
    gen_server:cast(?MODULE, {CastCommand, CState}).

handle_cast({{extendedQueryFromClient,
        {_, QueryId, {createTargetQuery, {_,SysProp,Prop}}}}, CState}, S) ->
    NProp    = [{Key,Val} || {'Prop', Key, Val} <- Prop],
    NSysProp = [{Key,Val} || {'Prop', Key, Val} <- SysProp],
    NSysProp2 = sysprop_guard(NSysProp),
    TargetId = monitor:target_new(NSysProp2, NProp),
    case snmp_enabled(NSysProp2) of
        true ->
            SInfoJob  = monitor:job_new({internal, update_snmp_system_info}, TargetId),
            IfInfoJob = monitor:job_new({internal, update_snmp_if_aliases},  TargetId),
            monitor:job_fire(SInfoJob),
            monitor:job_fire(IfInfoJob);
        false -> ok
    end,
    ReplyPDU = monitor_pdu:'PDU-MonitorPDU-fromServer-extendedReply'(
        QueryId, true, true, {string, TargetId}),
    supercast_channel:unicast(CState, [ReplyPDU]),
    {noreply, S};

handle_cast({{extendedQueryFromClient,
        {_, QueryId, {createNchecksQuery, {_,Target,Type,Props}}}}, CState}, S) ->
    Args = [{Key, Val} || {'Prop', Key, Val} <- Props],
    ProbeId = monitor:probe_new({nchecks,Type,Args}, Target),
    ReplyPDU = monitor_pdu:'PDU-MonitorPDU-fromServer-extendedReply'(
        QueryId, true, true, {string, ProbeId}),
    supercast_channel:unicast(CState, [ReplyPDU]),
    {noreply, S};

handle_cast({{extendedQueryFromClient,
        {_, QueryId, {createIfPerfQuery, {_,Target, Ifs}}}}, CState}, S) ->
    ProbeId = monitor:probe_new({snmp, if_perfs,Ifs}, Target),
    ReplyPDU = monitor_pdu:'PDU-MonitorPDU-fromServer-extendedReply'(
        QueryId, true, true, {string, ProbeId}),
    supercast_channel:unicast(CState, [ReplyPDU]),
    {noreply, S};

handle_cast({{extendedQueryFromClient,
        {_, QueryId, {elementInterfaceQuery, {_,SysProp,Prop}}}}, CState}, S) ->
    NProp    = [{Key,Val} || {'Prop', Key, Val} <- Prop],
    NSysProp = [{Key,Val} || {'Prop', Key, Val} <- SysProp],

    case walk_ifTable(NProp, NSysProp) of
        {ok, Val} ->
            io:format("val is: ~p~n",[Val]),
            ReplyPDU = monitor_pdu:'PDU-MonitorPDU-fromServer-extendedReply-snmpInterfacesInfo'(
                QueryId, true, true, Val),
            supercast_channel:unicast(CState, [ReplyPDU]);
        {error, timeout} ->
            ReplyPDU = monitor_pdu:'PDU-MonitorPDU-fromServer-extendedReply'(
                QueryId, false, true, {string, "timeout"}),
            supercast_channel:unicast(CState, [ReplyPDU]);
        {error, Reason} ->
            ReplyPDU = monitor_pdu:'PDU-MonitorPDU-fromServer-extendedReply'(
                QueryId, false, true, {string, Reason}),
            supercast_channel:unicast(CState, [ReplyPDU])
    end,
    {noreply, S};


handle_cast(R, S) ->
    error_logger:info_msg("unknown cast for command ~p ~p ~p~n", [?MODULE, ?LINE, R]),
    {noreply, S}.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([])                -> {ok, nostate}.
handle_call(_R, _F, S)  -> {noreply, S}.
handle_info(_I, S)      -> {noreply, S}.
terminate(_R, _S)       -> normal.
code_change(_O, S, _E)  -> {ok, S}.




sysprop_guard(NSysProp) ->
    case proplists:get_value("snmp_version", NSysProp) of
        undefined ->
            NSysProp;
        _ ->
            build_snmpConf(NSysProp)
    end.

build_snmpConf(NSysProp) ->
    io:format("~p~n",[NSysProp]),
    Default = ?DEFAULT_SNMP_PROPERTIES,
    build_snmpConf(NSysProp, Default).
build_snmpConf([], Default) -> Default;
build_snmpConf([{"snmp_port", Val}|R], Default) ->
    Port = erlang:list_to_integer(Val),
    NDefault = lists:keystore("snmp_port", 1, Default, {"snmp_port", Port}),
    build_snmpConf(R, NDefault);
build_snmpConf([{"snmp_timeout", Val}|R], Default) ->
    Timeout = erlang:list_to_integer(Val),
    NDefault = lists:keystore("snmp_timeout", 1, Default, {"snmp_timeout", Timeout}),
    build_snmpConf(R, NDefault);
build_snmpConf([{Key,Val}|R], Default) ->
    NDefault = lists:keystore(Key, 1, Default, {Key, Val}),
    build_snmpConf(R, NDefault).

walk_ifTable(Props,SProps) ->
    PortString = proplists:get_value("snmp_port", SProps),
    Port = erlang:list_to_integer(PortString),
    TimeoutString = proplists:get_value("snmp_timeout", SProps),
    Timeout = erlang:list_to_integer(TimeoutString),
    SnmpVer = proplists:get_value("snmp_version", SProps),
    Community = proplists:get_value("snmp_community", SProps, "public"),
    SecLevel = proplists:get_value("snmp_seclevel", SProps, "noAuthNoPriv"),
    SecName = proplists:get_value("snmp_usm_user", SProps, "undefined"),
    AuthProto = proplists:get_value("snmp_authproto", SProps, "MD5"),
    AuthKey = proplists:get_value("snmp_authkey", SProps, "undefined"),
    PrivProto = proplists:get_value("snmp_privproto", SProps, "DES"),
    PrivKey = proplists:get_value("snmp_privkey", SProps, "undefined"),
    Host = proplists:get_value("host", Props),

    case snmpman:register_element(?TMP_ELEMENT,
        [
            {host, Host},
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
            R = snmpman:walk_table(?TMP_ELEMENT, ?IF_INFO),
            snmpman:unregister_element(?TMP_ELEMENT),
            R;
        {error, Error} ->
            {error, Error}
    end.

snmp_enabled(SProps) ->
    case proplists:get_value("snmp_version", SProps) of
        undefined   -> false;
        _           -> true
    end.

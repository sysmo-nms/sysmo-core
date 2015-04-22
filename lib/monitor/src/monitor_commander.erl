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

% TODO handle_command might be spawned when no log is required.
% Actual lock is required for walk_ifTable (because of the temporary
% agent)

handle_command(Command, CState) ->
    % TODO check permissions here?
    gen_server:cast(?MODULE, {Command, CState}).

handle_cast({{"createTargetQuery", Contents}, CState}, S) ->
    % TODO check permissions
    {struct, Contents2} = proplists:get_value(<<"value">>, Contents),
    {struct, Prop}  = proplists:get_value(<<"properties">>, Contents2),
    {struct, SProp} = proplists:get_value(<<"sysProperties">>, Contents2),
    QueryId  = proplists:get_value(<<"queryId">>, Contents2),
    NProp    = [{binary_to_list(Key), maybe_str(Val)} || {Key,Val} <- Prop],
    NSysProp = [{binary_to_list(Key), maybe_str(Val)} || {Key,Val} <- SProp],
    NSysProp2 = sysprop_guard(NSysProp),
    TargetId = monitor:new_target(NSysProp2, NProp),
    case snmp_enabled(NSysProp2) of
        true ->
            SInfoJob  = monitor:new_job({internal, update_snmp_system_info}, TargetId),
            IfInfoJob = monitor:new_job({internal, update_snmp_if_aliases},  TargetId),
            monitor:fire_job(SInfoJob),
            monitor:fire_job(IfInfoJob);
        false -> ok
    end,
    ReplyPDU = monitor_pdu:simpleReply(QueryId, true, true, TargetId),
    supercast_channel:unicast(CState, [ReplyPDU]),
    {noreply, S};

handle_cast({{"createNchecksQuery", Contents}, CState}, S) ->
    % TODO check permissions
    {struct, Contents2} = proplists:get_value(<<"value">>, Contents),
    {struct, Prop}  = proplists:get_value(<<"properties">>, Contents2),
    Prop2 = [{binary_to_list(Key), maybe_str(Val)} || {Key, Val} <- Prop],
    Name    = binary_to_list(proplists:get_value(<<"name">>, Contents2)),
    Target  = binary_to_list(proplists:get_value(<<"target">>, Contents2)),
    QueryId = proplists:get_value(<<"queryId">>, Contents2),
    ProbeId = monitor:new_probe({nchecks_probe, Name, Prop2}, Target),
    ReplyPDU = monitor_pdu:simpleReply(QueryId, true, true, ProbeId),
    supercast_channel:unicast(CState, [ReplyPDU]),
    {noreply, S};

handle_cast({{"deleteProbeQuery", Contents}, CState}, S) ->
    % TODO check permissions
    {struct, Contents2} = proplists:get_value(<<"value">>, Contents),
    Probe   = binary_to_list(proplists:get_value(<<"name">>,  Contents2)),
    QueryId = proplists:get_value(<<"queryId">>,  Contents2),
    monitor:del_probe(Probe),
    ReplyPDU = monitor_pdu:simpleReply(QueryId, true, true, Probe),
    supercast_channel:unicast(CState, [ReplyPDU]),
    {noreply, S};

handle_cast({{"deleteTargetQuery", Contents}, CState}, S) ->
    % TODO check permissions
    {struct, Contents2}  = proplists:get_value(<<"value">>, Contents),
    Target  = binary_to_list(proplists:get_value(<<"name">>, Contents2)),
    QueryId = proplists:get_value(<<"queryId">>, Contents2),
    monitor:del_target(Target),
    ReplyPDU = monitor_pdu:simpleReply(QueryId, true, true, Target),
    supercast_channel:unicast(CState, [ReplyPDU]),
    {noreply, S};

handle_cast({{"forceProbeQuery", Contents}, CState}, S) ->
    % TODO check permissions
    {struct, Contents2}  = proplists:get_value(<<"value">>, Contents),
    Probe   = binary_to_list(proplists:get_value(<<"name">>, Contents2)),
    QueryId = proplists:get_value(<<"queryId">>, Contents2),
    monitor:force_probe(Probe),
    ReplyPDU = monitor_pdu:simpleReply(QueryId, true, true, Probe),
    supercast_channel:unicast(CState, [ReplyPDU]),
    {noreply, S};

handle_cast(R, S) ->
    error_logger:info_msg("unknown cast for command ~p ~p ~p~n", [?MODULE, ?LINE, R]),
    {noreply, S}.

%%----------------------------------------------------------------------------
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([])                -> {ok, nostate}.
handle_call(_R, _F, S)  -> {noreply, S}.
handle_info(_I, S)      -> {noreply, S}.
terminate(_R, _S)       -> normal.
code_change(_O, S, _E)  -> {ok, S}.
%%----------------------------------------------------------------------------




sysprop_guard(NSysProp) ->
    case proplists:get_value("snmp_version", NSysProp) of
        undefined ->
            NSysProp;
        _ ->
            build_snmpConf(NSysProp)
    end.

build_snmpConf(NSysProp) ->
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

snmp_enabled(SProps) ->
    case proplists:get_value("snmp_version", SProps) of
        undefined   -> false;
        _           -> true
    end.

maybe_str(Val) when is_binary(Val) -> binary_to_list(Val);
maybe_str(Other) -> Other.

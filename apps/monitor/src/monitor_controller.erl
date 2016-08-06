%%=
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
%%=
% @private
-module(monitor_controller).
-include("monitor.hrl").
-include_lib("common_hrl/include/logs.hrl").
-behaviour(supercast_controller).
-behaviour(gen_server).

% GEN_SERVER
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-export([start_link/0]).

% SUPERCAST_COMMANDER
-export([handle_command/2]).

% TODO remove handle_cast for most of the commands, and if crash, crash the
% tcp_client process only (see supercast TODO.md).

handle_command(Command, CState) ->
    % TODO check permissions here?
    gen_server:cast(?MODULE, {Command, CState}).

handle_cast({{"createTargetQuery", Contents}, CState}, S) ->
    ?LOG_INFO("Create target query", Contents),
    QueryId   = proplists:get_value(<<"queryId">>, Contents),
    Contents2 = proplists:get_value(<<"value">>,   Contents),
    Prop      = proplists:get_value(<<"properties">>,    Contents2),
    SProp     = proplists:get_value(<<"sysProperties">>, Contents2),
    NProp     = [{binary_to_list(Key), maybe_str(Val)} || {Key,Val} <- Prop],
    NSysProp  = [{binary_to_list(Key), maybe_str(Val)} || {Key,Val} <- SProp],
    {NProp2, NSysProp2} = sysprop_guard(NProp,NSysProp),

    % can fail if USM user is incorrect
    case monitor:new_target(NSysProp2, NProp2) of
        {error, Error} ->
            ?LOG_ERROR("Create target error: ", Error),
            ReplyPDU = monitor_pdu:simpleReply(QueryId, false, true, Error);
        TargetId ->
            ReplyPDU = monitor_pdu:simpleReply(QueryId, true, true, TargetId),
            case snmp_enabled(NProp2) of
                true ->
                    SInfoJob  = monitor:new_job(
                        update_snmp_system_info, TargetId),
                    IfInfoJob = monitor:new_job(
                        update_snmp_if_aliases,  TargetId),
                    monitor:fire_job(SInfoJob),
                    monitor:fire_job(IfInfoJob);
                false -> ok
            end
    end,
    supercast_proc:send_unicast(?MASTER_CHANNEL, CState, [ReplyPDU]),
    {noreply, S};

handle_cast({{"createNchecksQuery", Contents}, CState}, S) ->
    ?LOG_INFO("create probe query", Contents),
    QueryId   = proplists:get_value(<<"queryId">>, Contents),
    Contents2 = proplists:get_value(<<"value">>,   Contents),
    Prop      = proplists:get_value(<<"properties">>, Contents2),
    Prop2 = [{binary_to_list(Key), maybe_str(Val)} || {Key, Val} <- Prop],

    Class   = binary_to_list(proplists:get_value(<<"class">>,      Contents2)),
    Id      = binary_to_list(proplists:get_value(<<"identifier">>, Contents2)),
    Target  = binary_to_list(proplists:get_value(<<"target">>,     Contents2)),
    Display = binary_to_list(proplists:get_value(<<"display">>,    Contents2)),

    ProbeId = monitor:new_probe(
        {nchecks_probe, Id, Class, Display, Prop2}, Target),

    ReplyPDU = monitor_pdu:simpleReply(QueryId, true, true, ProbeId),
    supercast_proc:send_unicast(?MASTER_CHANNEL, CState, [ReplyPDU]),
    {noreply, S};

handle_cast({{"deleteProbeQuery", Contents}, CState}, S) ->
    ?LOG_INFO("delete probe query", Contents),
    QueryId   = proplists:get_value(<<"queryId">>, Contents),
    Contents2 = proplists:get_value(<<"value">>,   Contents),
    Probe     = binary_to_list(proplists:get_value(<<"name">>,  Contents2)),
    monitor:del_probe(Probe),
    ReplyPDU = monitor_pdu:simpleReply(QueryId, true, true, Probe),
    supercast_proc:send_unicast(?MASTER_CHANNEL, CState, [ReplyPDU]),
    {noreply, S};

handle_cast({{"deleteTargetQuery", Contents}, CState}, S) ->
    ?LOG_INFO("delete target query", Contents),
    QueryId   = proplists:get_value(<<"queryId">>, Contents),
    Contents2 = proplists:get_value(<<"value">>,   Contents),
    Target    = binary_to_list(proplists:get_value(<<"name">>, Contents2)),
    monitor:del_target(Target),
    ReplyPDU = monitor_pdu:simpleReply(QueryId, true, true, Target),
    supercast_proc:send_unicast(?MASTER_CHANNEL, CState, [ReplyPDU]),
    {noreply, S};

handle_cast({{"forceProbeQuery", Contents}, CState}, S) ->
    ?LOG_INFO("force probe query", Contents),
    QueryId   = proplists:get_value(<<"queryId">>, Contents),
    Contents2 = proplists:get_value(<<"value">>,   Contents),
    Probe   = binary_to_list(proplists:get_value(<<"name">>, Contents2)),
    monitor:force_probe(Probe),
    ReplyPDU = monitor_pdu:simpleReply(QueryId, true, true, Probe),
    supercast_proc:send_unicast(?MASTER_CHANNEL, CState, [ReplyPDU]),
    {noreply, S};

handle_cast({{"ncheckHelperQuery", Contents}, CState}, S) ->
    ?LOG_INFO("helper query", Contents),
    QueryId   = proplists:get_value(<<"queryId">>, Contents),
    Contents2 = proplists:get_value(<<"value">>,   Contents),
    Target    = binary_to_list(proplists:get_value(<<"target">>, Contents2)),
    Class     = binary_to_list(proplists:get_value(<<"class">>,  Contents2)),
    Props     = get_snmp_args(Target),
    case (catch j_server_nchecks:helper(Class, Props)) of
        {ok, Reply} ->
            ReplyPDU = monitor_pdu:nchecksHelperReply(QueryId, Class, Reply),
            supercast_proc:send_unicast(?MASTER_CHANNEL, CState, [ReplyPDU]);
        Error ->
            ErrorStr = io_lib:format("~p", [Error]),
            ReplyPDU = monitor_pdu:simpleReply(QueryId, false, true, ErrorStr),
            supercast_proc:send_unicast(?MASTER_CHANNEL, CState, [ReplyPDU])
    end,
    {noreply, S};

handle_cast(_R, S) ->
    % TODO crash the client
    ?LOG_INFO("unknown cast for command", _R),
    {noreply, S}.

%%----------------------------------------------------------------------------
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([])                -> {ok, nostate}.
handle_call(_R, _F, S)  -> {noreply, S}.
handle_info(_I, S)      -> {noreply, S}.
terminate(_R, _S)       -> normal.
code_change(_O, S, _E)  -> {ok, S}.
%%----------------------------------------------------------------------------

get_snmp_args(TargetName) ->
    [Target]        = monitor_data_master:get(target, TargetName),
    TargetSysProp   = Target#target.sys_properties,
    TargetProp      = Target#target.properties,
    [
        {"target_id", TargetName},
        {_,_} = lists:keyfind("host",           1, TargetProp),
        {_,_} = lists:keyfind("snmp_port",      1, TargetSysProp),
        {_,_} = lists:keyfind("snmp_version",   1, TargetSysProp),
        {_,_} = lists:keyfind("snmp_seclevel",  1, TargetSysProp),
        {_,_} = lists:keyfind("snmp_community", 1, TargetSysProp),
        {_,_} = lists:keyfind("snmp_usm_user",  1, TargetSysProp),
        {_,_} = lists:keyfind("snmp_authkey",   1, TargetSysProp),
        {_,_} = lists:keyfind("snmp_authproto", 1, TargetSysProp),
        {_,_} = lists:keyfind("snmp_privkey",   1, TargetSysProp),
        {_,_} = lists:keyfind("snmp_privproto", 1, TargetSysProp),
        {_,_} = lists:keyfind("snmp_timeout",   1, TargetSysProp),
        {_,_} = lists:keyfind("snmp_retries",   1, TargetSysProp)
    ].



sysprop_guard(NProp, NSysProp) ->
    case proplists:get_value("snmp_version", NSysProp) of
        undefined ->
            {NProp, NSysProp};
        _ ->
            {[{"isSnmpAware", "true"} | NProp], build_snmpConf(NSysProp)}
    end.

build_snmpConf(NSysProp) ->
    Default = ?DEFAULT_SNMP_PROPERTIES,
    build_snmpConf(NSysProp, Default).
build_snmpConf([], Default) -> Default;
build_snmpConf([{Key,Val}|R], Default) ->
    NDefault = lists:keystore(Key, 1, Default, {Key, Val}),
    build_snmpConf(R, NDefault).

snmp_enabled(Props) ->
    case proplists:get_value("isSnmpAware", Props) of
        undefined   -> false;
        _           -> true
    end.

maybe_str(Val) when is_binary(Val) -> binary_to_list(Val);
maybe_str(Other) -> Other.

% This file is part of "Enms" (http://sourceforge.net/projects/enms/)
% Copyright (C) 2012 <Sébastien Serre sserre.bx@gmail.com>
% 
% Enms is a Network Management System aimed to manage and monitor SNMP
% targets, monitor network hosts and services, provide a consistent
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
-module(bmonitor_probe_snmp).
-behaviour(beha_monitor_probe).
-include_lib("snmp/include/snmp_types.hrl").
-include("include/monitor.hrl").

%% beha_monitor_probe exports
-export([
    init/2,
    exec/1,
    info/0
]).

-record(state, {
    agent,
    oids,
    request_oids,
    timeout
}).

info() -> {ok, "snmp get and walk module"}.

init(Target, Probe) ->

    TargetName  = Target#target.id,
    AgentName   = atom_to_list(TargetName),

    TargetIp    = Target#target.ip,
    {ok, Ip}    = inet:parse_address(TargetIp),

    Conf        = Probe#probe.monitor_probe_conf,
    Port        = Conf#snmp_conf.port,
    Version     = Conf#snmp_conf.version,
    Community   = Conf#snmp_conf.community,
    Oids        = Conf#snmp_conf.oids,

    case snmp_manager:agent_registered(AgentName) of
        true  -> ok;
        false ->
            SnmpArgs = [
                {engine_id, "none"},
                {address,   Ip},
                {port  ,    Port},
                {version,   Version},
                {community, Community}
            ],
            snmp_manager:register_agent(AgentName, SnmpArgs)
    end,

    {ok, #state{
            agent           = AgentName,
            oids            = Oids,
            request_oids    = [Oid || {_, Oid} <- Oids],
            timeout         = Probe#probe.timeout
        }
    }.

exec(State) ->

    Agent           = State#state.agent,
    Request         = State#state.request_oids,
    Oids            = State#state.oids,
    Timeout         = State#state.timeout,

    {_, MicroSec1}  = sys_timestamp(),
    % TODO use snmp_manager:bulk_walk
    Reply           = snmp_manager:sync_get(Agent, Request, Timeout),
    {_, MicroSec2}  = sys_timestamp(),

    case Reply of
        {error, _Error} = R ->
            KV = [{"status",'CRITICAL'},{"sys_latency",MicroSec2 - MicroSec1}],
            OR = to_string(R),
            S  = 'CRITICAL',
            PR = #probe_return{
                status          = S,
                original_reply  = OR,
                key_vals        = KV,
                timestamp       = MicroSec2},
            {ok, State, PR};
        {ok, SnmpReply, _Remaining} ->
            PR      = eval_snmp_return(SnmpReply, Oids),
            KV      = PR#probe_return.key_vals,
            KV2     = [{"sys_latency", MicroSec2 - MicroSec1} | KV],
            PR2     = PR#probe_return{
                timestamp = MicroSec2,
                key_vals  = KV2},
            {ok, State, PR2}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTILS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @private
% @doc
% from snmpm documentation: snmpm, Common Data Types 
% snmp_reply() = {error_status(), error_index(), varbinds()}
% @end
eval_snmp_return({noError, _, VarBinds}, Oids) ->
    KeyVals = [
        {Key, (lists:keyfind(Oid, 2, VarBinds))#varbind.value} || 
        {Key, Oid} <- Oids
    ],
    #probe_return{
        status          = 'OK',
        original_reply  = to_string(VarBinds),
        key_vals        = [{"status", 'OK'} | KeyVals]
    }.

to_string(Term) ->
    lists:flatten(io_lib:format("~p~n", [Term])).

sys_timestamp() ->
    {Meg, Sec, Micro} = os:timestamp(),
    Seconds      = Meg      * 1000000 + Sec,
    Microseconds = Seconds  * 1000000 + Micro,
    {Seconds, Microseconds}.

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
% @doc
% TODO
% This is a special probe.
% It is effective on all snmp enabled elements without configuration.
% At regular intervals it will fetch:
%   - interfaces infos (snmp_manager:get_mib2_interfaces),
%   - interfaces performances (snmp_manager:get_mib2_interfaces_perfs),
%   and log to RRDs,
%   - system infos (snmp_manager:get_mib2_system/1)
% @end

-module(bmonitor_probe_standard_snmp).
-behaviour(beha_monitor_probe).
-include_lib("snmp/include/snmp_types.hrl").
-include("../../include/monitor.hrl").
-define(SNMP_USER, "noctopus_snmpm_user").

%% beha_monitor_probe exports
-export([
    init/1,
    exec/1,
    info/0]).

init(#ps_state{
        probe  = #probe{monitor_probe_conf = Conf},
        target = #target{id = Name}
    } = S) ->
    #snmp_conf{
        ip          = IpString,
        port        = Port,
        version     = Version,
        community   = Community
    } = Conf,
    {ok, Ip} = inet:parse_address(IpString),
    SnmpConf = [
        {engine_id, "none"},
        {address,   Ip},
        {port  ,    Port},
        {version,   Version},
        {community, Community}
    ],
    Agent = atom_to_list(Name),
    case agent_is_registered(Agent) of
        true -> S;
        false ->
            snmpm:register_agent(?SNMP_USER, Agent, SnmpConf),
            S
    end.

exec({_,#probe{
            monitor_probe_conf = #snmp_conf{
                agent_name  =  Agent,
                oids        =  Oids
            },
            timeout = Timeout
        }
    }) -> 
    Request = [Oid || {_, Oid} <- Oids],
    {_, MicroSec1} = sys_timestamp(),
    Rep = snmpm:sync_get(?SNMP_USER, Agent, Request, Timeout * 1000),
    {_, MicroSec2} = sys_timestamp(),
    case Rep of
        {error, _Error} = R ->
            #probe_return{
                status          = 'CRITICAL',
                original_reply  = to_string(R),
                key_vals        = [ {"status", 'CRITICAL'}, 
                                    {"sys_latency", MicroSec2 - MicroSec1}],
                timestamp       = MicroSec2
            };
        {ok, SnmpReply, _Remaining} ->
            % from snmpm documentation: snmpm, Common Data Types 
            % snmp_reply() = {error_status(), error_index(), varbinds()}
            % {_ErrStatus, _ErrId, VarBinds} = SnmpReply,
            % lists:foreach(fun(X) ->
                % io:format("rep is noError ~p~n",[X#varbind.value])
            % end, VarBinds)
            #probe_return{key_vals = KV} = PR = eval_snmp_return(SnmpReply, Oids),
            PR#probe_return{
                timestamp = MicroSec2,
                key_vals  = [{"sys_latency", MicroSec2 - MicroSec1} | KV]
            }
    end.

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

info() -> {ok, "snmp get and walk module"}.

to_string(Term) ->
    lists:flatten(io_lib:format("~p~n", [Term])).

agent_is_registered(Agent) ->
    lists:member(Agent, snmpm:which_agents(?SNMP_USER)).

sys_timestamp() ->
    {Meg, Sec, Micro} = os:timestamp(),
    Seconds      = Meg * 1000000 + Sec,
    Microseconds = Seconds * 1000000 + Micro,
    {Seconds, Microseconds}.

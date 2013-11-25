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
-module(btracker_probe_snmp).
-behaviour(beha_tracker_probe).
-behaviour(snmpm_user).
-include_lib("snmp/include/snmp_types.hrl").
-include("../../include/tracker.hrl").
-define(SNMP_USER, "tracker_probe_user").

%% beha_tracker_probe exports
-export([
    init/1,
    exec/1,
    info/0]).

%% snmpm_user exports
-export([
    handle_error/3,
    handle_agent/5,
    handle_pdu/4,
    handle_trap/3,
    handle_inform/3,
    handle_report/3
]).

init(#probe_server_state{
        probe  = #probe{tracker_probe_conf = Conf},
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
            tracker_probe_conf = #snmp_conf{
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


%% snmpm_user behaviour
handle_error(_ReqId, _Reason, _UserData) ->
    io:format("handle_error ~p~n", [?MODULE]),
    ignore.

handle_agent(_Addr, _Port, _Type, _SnmpInfo, _UserData) ->
    io:format("handle_agent ~p~n", [?MODULE]),
    ignore.

handle_pdu(_TargetName, _ReqId, SnmpResponse, _UserData) ->
    io:format("handle_pdu ~p ~p~n", [?MODULE,SnmpResponse]),
    ignore.

handle_trap(_TargetName, _SnmpTrapInfo, _UserData) ->
    io:format("handle_trap ~p~n", [?MODULE]),
    ignore.

handle_inform(_TargetName, _SnmpInform, _UserData) ->
    io:format("handle_inform ~p~n", [?MODULE]),
    ignore.

handle_report(_TargetName, _SnmpReport, _UserData) ->
    io:format("handle_report ~p~n", [?MODULE]),
    ignore.


to_string(Term) ->
    lists:flatten(io_lib:format("~p~n", [Term])).

agent_is_registered(Agent) ->
    lists:member(Agent, snmpm:which_agents(?SNMP_USER)).

sys_timestamp() ->
    {Meg, Sec, Micro} = os:timestamp(),
    Seconds      = Meg * 1000000 + Sec,
    Microseconds = Seconds * 1000000 + Micro,
    {Seconds, Microseconds}.

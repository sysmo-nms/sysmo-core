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
-include("include/monitor.hrl").

%% beha_monitor_probe exports
-export([
    init/2,
    exec/1,
    info/0
]).

-record(varbind, {
    oid,
    type,
    value
}).

-record(state, {
    agent,
    oids,
    request_oids,
    method
}).

info() -> {ok, "snmp get and walk module"}.

init(Target, Probe) ->

    TargetName  = Target#target.id,
    AgentName   = atom_to_list(TargetName),

    Conf        = Probe#probe.monitor_probe_conf,
    Method      = Conf#snmp_probe_conf.method,
    Oids        = Conf#snmp_probe_conf.oids,


    case snmpman:element_registered(AgentName) of
        true  -> ok;
        false ->
            Ip    = Target#target.ip,
            IpVer = Target#target.ip_version,

            Port        = Conf#snmp_probe_conf.port,
            Version     = Conf#snmp_probe_conf.version,
            SecLevel    = Conf#snmp_probe_conf.seclevel,
            Community   = Conf#snmp_probe_conf.community,
            AuthKey     = Conf#snmp_probe_conf.authkey,
            AuthProto   = Conf#snmp_probe_conf.authproto,
            PrivKey     = Conf#snmp_probe_conf.privkey,
            PrivProto   = Conf#snmp_probe_conf.privproto,
            Retries     = Conf#snmp_probe_conf.retries,
            UsmUser     = Conf#snmp_probe_conf.usm_user,
            Timeout     = Probe#probe.timeout,
            SnmpArgs = [
                {ip_address,    Ip},
                {ip_version,    IpVer},
                {timeout,       Timeout},
                {port  ,        Port},
                {snmp_version,  Version},
                {security_level,SecLevel},
                {community,     Community},
                {auth_key,      AuthKey},
                {auth_proto,    AuthProto},
                {priv_key,      PrivKey},
                {priv_proto,    PrivProto},
                {retries,       Retries},
                {security_name, UsmUser}
            ],
            io:format("should register element~p ~p", [AgentName, SnmpArgs]),
            snmpman:register_element(AgentName, SnmpArgs)
    end,

    {ok,
        #state{
            agent           = AgentName,
            oids            = Oids,
            request_oids    = [Oid || {_, Oid} <- Oids],
            method          = Method
        }
    }.

exec(#state{method = get} = State) ->

    Agent           = State#state.agent,
    Request         = State#state.request_oids,
    Oids            = State#state.oids,

    {_, MicroSec1}  = sys_timestamp(),
    Reply = snmpman:get(Agent, Request),
    {_, MicroSec2}  = sys_timestamp(),

    case Reply of
        {error, _Error} = R ->
            error_logger:info_msg("snmp fail ~p ~p ~p for agent ~p", [?MODULE, ?LINE, R, Agent]),
            KV = [{"status",'CRITICAL'},{"sys_latency",MicroSec2 - MicroSec1}],
            OR = to_string(R),
            S  = 'CRITICAL',
            PR = #probe_return{
                status          = S,
                original_reply  = OR,
                key_vals        = KV,
                timestamp       = MicroSec2},
            {ok, State, PR};
        {ok, SnmpReply} ->
            PR  = eval_snmp_get_return(SnmpReply, Oids),
            KV  = PR#probe_return.key_vals,
            KV2 = [{"sys_latency", MicroSec2 - MicroSec1} | KV],
            PR2 = PR#probe_return{
                timestamp = MicroSec2,
                key_vals  = KV2},
            {ok, State, PR2}
    end;

exec(#state{method = {walk_table, Table, PropRet}} = State) ->

    Agent           = State#state.agent,
    %Request         = State#state.request_oids,
    %Oids            = State#state.oids,
    %Method          = State#state.method,

    {_, MicroSec1}  = sys_timestamp(),
    Reply = snmpman:walk_table(Agent, Table),
    {ReplyT, MicroSec2}  = sys_timestamp(),

    case Reply of
        {error, _Error} = R ->
            error_logger:info_msg("snmp fail ~p ~p ~p for agent ~p", [?MODULE, ?LINE, R, Agent]),
            KV = [{"status",'CRITICAL'},{"sys_latency",MicroSec2 - MicroSec1}],
            OR = to_string(R),
            S  = 'CRITICAL',
            PR = #probe_return{
                status          = S,
                original_reply  = OR,
                reply_tuple     = ignore,
                key_vals        = KV,
                timestamp       = ReplyT},
            {ok, State, PR};
        {ok, {table, SnmpReply}} ->
            KV  = [{"status",'OK'},{"sys_latency", MicroSec2 - MicroSec1}],
            KV2 = set_walk_prop_ret(PropRet, SnmpReply, []),
            PR = #probe_return{
                timestamp       = ReplyT,
                reply_tuple     = SnmpReply,
                status          = 'OK',
                key_vals        = lists:concat([KV,KV2]),
                original_reply  = to_string(SnmpReply)
            },
            {ok, State, PR}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTILS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @private
set_walk_prop_ret([], _, KV) ->
    KV;
set_walk_prop_ret([{MacStr, MacElement, RepElement}|T], SnmpReply, KV) ->
    KV2 = set_walk_prop_ret_replace(MacStr,MacElement,RepElement, SnmpReply, []),
    set_walk_prop_ret(T, SnmpReply, lists:concat([KV, KV2])).

set_walk_prop_ret_replace(_,_,_,[],K) -> K;
set_walk_prop_ret_replace(MStr,MElem,MRep,[H|T],K) ->
    MMacVal     = element(MElem,    H),
    MRepVal     = element(MRep,     H),
    %Key = re:replace(MStr, Rx, int_to_string(MMacVal), [{return,list}]),
    Key = lists:concat([MStr, int_to_string(MMacVal)]),
    Kv = {Key, MRepVal},
    set_walk_prop_ret_replace(MStr,MElem,MRep,T,[Kv|K]).


% @private
eval_snmp_get_return({varbinds, VarBinds}, Oids) ->
    eval_snmp_return(VarBinds, Oids).

%eval_snmp_walk_return(VarBinds, Oids) ->
    %OidsN = [{K, lists:droplast(O)} || {K, O} <- Oids],
    %eval_snmp_return(VarBinds, OidsN).

eval_snmp_return(VarBinds, Oids) ->
    KeyVals = [
        {Key, (lists:keyfind(Oid, 2, VarBinds))#varbind.value} || 
        {Key, Oid} <- Oids
    ],
    #probe_return{
        status          = 'OK',
        original_reply  = to_string(VarBinds),
        key_vals        = [{"status", 'OK'} | KeyVals]
    }.

int_to_string(Term) when is_integer(Term) ->
    lists:flatten(io_lib:format("~p", [Term]));
int_to_string(Term) -> Term.

to_string(Term) ->
    lists:flatten(io_lib:format("~p", [Term])).

sys_timestamp() ->
    {Meg, Sec, Micro} = os:timestamp(),
    Seconds      = Meg      * 1000000 + Sec,
    Microseconds = Seconds  * 1000000 + Micro,
    {Seconds, Microseconds}.

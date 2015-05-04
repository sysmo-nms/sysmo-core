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

% PDUS
-module(monitor_pdu).
-include("include/monitor.hrl").
-export([
    probeReturn/4,
    infoProbeCreate/1,
    infoProbeUpdate/1,
    deleteTarget/1,
    deleteProbe/1,
    infoTargetCreate/1,
    infoTargetUpdate/1,
    simpleReply/4,
    nchecksHelperReply/3,

    nchecksSimpleUpdateMessage/3,
    nchecksSimpleDumpMessage/3,
    nchecksTableUpdateMessage/3,
    nchecksTableDumpMessage/3
]).

nchecksSimpleUpdateMessage(Probe, Ts, Updates) ->
    Up = [{list_to_binary(K), V} || {K,V} <- Updates],
    {struct,
        [
            {<<"from">>, <<"monitor">>},
            {<<"type">>, <<"nchecksSimpleUpdateMessage">>},
            {<<"value">>, 
                {struct, [
                    {<<"name">>,        list_to_binary(Probe)},
                    {<<"timestamp">>,   Ts},
                    {<<"rrdupdates">>,  {struct, Up}}
                ]}
            }
        ]
    }.

nchecksSimpleDumpMessage(Probe, DumpDir, RrdFile) ->
    {struct,
        [
            {<<"from">>, <<"monitor">>},
            {<<"type">>, <<"nchecksSimpleDumpMessage">>},
            {<<"value">>, 
                {struct, [
                    {<<"name">>,        list_to_binary(Probe)},
                    {<<"httpDumpDir">>, list_to_binary(DumpDir)},
                    {<<"rrdFile">>,     list_to_binary(RrdFile)}
                ]}
            }
        ]
    }.

nchecksTableUpdateMessage(Probe, Ts, Updates) ->
    Up = lists:map(fun({Idx, PropList}) ->
        {
            list_to_binary(Idx),
            {struct,
                [{list_to_binary(K), V} || {K,V} <- PropList]
            }
        }
    end, Updates),
    {struct,
        [
            {<<"from">>, <<"monitor">>},
            {<<"type">>, <<"nchecksTableUpdateMessage">>},
            {<<"value">>, 
                {struct, [
                    {<<"name">>,        list_to_binary(Probe)},
                    {<<"timestamp">>,   Ts},
                    {<<"rrdupdates">>,  {struct, Up}}
                ]}
            }
        ]
    }.


nchecksTableDumpMessage(Probe, DumpDir, ElemToFile) ->
    ElToFile = [{list_to_binary(A), list_to_binary(B)} || {A,B} <- ElemToFile],
    {struct,
        [
            {<<"from">>, <<"monitor">>},
            {<<"type">>, <<"nchecksTableDumpMessage">>},
            {<<"value">>, 
                {struct, [
                    {<<"name">>,        list_to_binary(Probe)},
                    {<<"httpDumpDir">>, list_to_binary(DumpDir)},
                    {<<"elementToFile">>,   {struct, ElToFile}}
                ]}
            }
        ]
    }.

simpleReply(QueryId, Status, Last, Msg) ->
    {struct,
        [
            {<<"from">>, <<"monitorUser">>},
            {<<"type">>, <<"reply">>},
            {<<"value">>, {struct, [
                {<<"queryId">>, QueryId},   
                {<<"status">>,  Status},
                {<<"last">>,    Last},
                {<<"reply">>,   list_to_binary(Msg)}]}}
        ]
    }.

nchecksHelperReply(QueryId, Class, Reply) ->
    {struct,
        [
            {<<"from">>, <<"monitorUser">>},
            {<<"type">>, <<"reply">>},
            {<<"value">>, {struct, [
                {<<"queryId">>, QueryId},   
                {<<"class">>,   list_to_binary(Class)},
                {<<"reply">>,   {json, Reply}}]}}
        ]
    }.


deleteTarget(TargetName) ->
    {struct,
        [
            {<<"from">>, <<"monitor">>},
            {<<"type">>, <<"deleteTarget">>},
            {<<"value">>, {struct, [
                {<<"name">>, list_to_binary(TargetName)}]}}
        ]
    }.
 
deleteProbe(Probe) ->
    #probe{name=Name,belong_to=Target} = Probe,
    {struct,
        [
            {<<"from">>, <<"monitor">>},
            {<<"type">>, <<"deleteProbe">>},
            {<<"value">>, {struct, [
                {<<"name">>,   list_to_binary(Name)},
                {<<"target">>, list_to_binary(Target)}]}}
        ]
    }.
 
probeReturn(ProbeReturn, Target, Probe, NextReturn) ->
    KeyValStr = make_key_values(ProbeReturn#probe_return.key_vals),
    JKeyVal = [{list_to_binary(Key), list_to_binary(Val)} || {Key, Val} <- KeyValStr],
    {struct,
        [
            {<<"from">>, <<"monitor">>},
            {<<"type">>, <<"probeReturn">>},
            {<<"value">>, {struct, [
                {<<"target">>,      list_to_binary(Target)},
                {<<"name">>,        list_to_binary(Probe)},
                {<<"status">>,      list_to_binary(ProbeReturn#probe_return.status)},
                {<<"replyString">>, list_to_binary(ProbeReturn#probe_return.reply_string)},
                {<<"replyCode">>,   ProbeReturn#probe_return.reply_code},
                {<<"timestamp">>,   ProbeReturn#probe_return.timestamp},
                {<<"keyVals">>,     {struct, JKeyVal}},
                {<<"nextReturn">>,  NextReturn}
            ]}}
        ]
    }.
   
infoTargetCreate(Target) -> infoTarget(Target, <<"create">>).
infoTargetUpdate(Target) -> infoTarget(Target, <<"update">>).
infoTarget(#target{name=Name, properties=Prop}, InfoType) ->
    JProp = [{list_to_binary(Key), maybe_str(Val)} || {Key,Val} <- Prop],
    {struct,
        [
            {<<"from">>, <<"monitor">>},
            {<<"type">>, <<"infoTarget">>},
            {<<"value">>, {struct, [
                {<<"name">>,          list_to_binary(Name)},
                {<<"properties">>,    {struct, JProp}},
                {<<"sysProperties">>, {struct, []}},
                {<<"infoType">>,      InfoType}]}
            }
        ]
    }.


maybe_str(Val) when is_integer(Val) -> Val;
maybe_str(Val) when is_float(Val) -> Val;
maybe_str(Val) -> list_to_binary(Val).
    
infoProbeCreate(Probe) -> infoProbe(Probe, <<"create">>).
infoProbeUpdate(Probe) -> infoProbe(Probe, <<"update">>).
infoProbe(Probe, InfoType) ->
    #probe{
        permissions         = #perm_conf{read = R, write = W},
        module_config       = ProbeConf } = Probe,
    
    JR = [list_to_binary(G) || G <- R],
    JW = [list_to_binary(G) || G <- W],

    case is_record(ProbeConf, nchecks_probe_conf) of
        true ->
            {_,Class,_} = ProbeConf;
        false ->
            Class = gen_str_probe_conf(ProbeConf)
    end,

    {struct,
        [
            {<<"from">>, <<"monitor">>},
            {<<"type">>, <<"infoProbe">>},
            {<<"value">>, {struct, [
                {<<"target">>,      list_to_binary(Probe#probe.belong_to)},
                {<<"name">>,        list_to_binary(Probe#probe.name)},
                {<<"descr">>,       list_to_binary(Probe#probe.description)},
                {<<"perm">>,        {struct, [{<<"read">>, {array, JR}}, {<<"write">>, {array, JW}}]}},
                {<<"probeMod">>,    atom_to_binary(Probe#probe.module, utf8)},
                {<<"probeClass">>,  list_to_binary(Class)},
                {<<"status">>,      list_to_binary(Probe#probe.status)},
                {<<"timeout">>,     Probe#probe.timeout},
                {<<"step">>,        Probe#probe.step},
                {<<"active">>,      Probe#probe.active},
                {<<"infoType">>,      InfoType}]}
            }
        ]
    }.
            

% UTILS
gen_str_probe_conf(Conf) ->
    lists:flatten(io_lib:format("~p", [Conf])).


make_key_values(K) ->
    make_key_values(K, []).
make_key_values([], S) ->
    S;
make_key_values([{K,V} | T], S) when is_list(V) ->
    make_key_values(T, [{K, V} | S]);
make_key_values([{K,V} | T], S) when is_integer(V) ->
    make_key_values(T, [{K, integer_to_list(V)} | S]);
make_key_values([{K,V} | T], S) when is_float(V) ->
    make_key_values(T, [{K, float_to_list(V, [{decimals, 10}])} | S]);
make_key_values([{K,V} | T], S) when is_atom(V) ->
    make_key_values(T, [{K, atom_to_list(V)} | S]).

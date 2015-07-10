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
    Up = [{char_to_binary(K), V} || {K,V} <- Updates],
    {struct,
        [
            {<<"from">>, char_to_binary(Probe)},
            {<<"type">>, <<"nchecksSimpleUpdateMessage">>},
            {<<"value">>,
                {struct, [
                    {<<"name">>,       char_to_binary(Probe)},
                    {<<"timestamp">>,  Ts},
                    {<<"rrdupdates">>, {struct, Up}}
                ]}
            }
        ]
    }.

nchecksSimpleDumpMessage(Probe, DumpDir, RrdFile) ->
    {struct,
        [
            {<<"from">>, char_to_binary(Probe)},
            {<<"type">>, <<"nchecksSimpleDumpMessage">>},
            {<<"value">>,
                {struct, [
                    {<<"name">>,        char_to_binary(Probe)},
                    {<<"httpDumpDir">>, char_to_binary(DumpDir)},
                    {<<"rrdFile">>,     char_to_binary(RrdFile)}
                ]}
            }
        ]
    }.

nchecksTableUpdateMessage(Probe, Ts, Updates) ->
    Up = lists:map(fun({Idx, PropList}) ->
        {
            char_to_binary(Idx),
            {struct,
                [{char_to_binary(K), V} || {K,V} <- PropList]
            }
        }
    end, Updates),
    {struct,
        [
            {<<"from">>, char_to_binary(Probe)},
            {<<"type">>, <<"nchecksTableUpdateMessage">>},
            {<<"value">>,
                {struct, [
                    {<<"name">>,       char_to_binary(Probe)},
                    {<<"timestamp">>,  Ts},
                    {<<"rrdupdates">>, {struct, Up}}
                ]}
            }
        ]
    }.


nchecksTableDumpMessage(Probe, DumpDir, ElemToFile) ->
    ElToFile = [{char_to_binary(A), char_to_binary(B)} || {A,B} <- ElemToFile],
    {struct,
        [
            {<<"from">>, char_to_binary(Probe)},
            {<<"type">>, <<"nchecksTableDumpMessage">>},
            {<<"value">>,
                {struct, [
                    {<<"name">>,          char_to_binary(Probe)},
                    {<<"httpDumpDir">>,   char_to_binary(DumpDir)},
                    {<<"elementToFile">>, {struct, ElToFile}}
                ]}
            }
        ]
    }.

simpleReply(QueryId, Status, Last, Msg) ->
    {struct,
        [
            {<<"from">>,    <<"monitorUser">>},
            {<<"type">>,    <<"reply">>},
            {<<"queryId">>, QueryId},
            {<<"lastPdu">>, Last},
            {<<"value">>, {struct, [
                {<<"status">>, Status},
                {<<"reply">>,  char_to_binary(Msg)}]}}
        ]
    }.

nchecksHelperReply(QueryId, Class, Reply) ->
    {struct,
        [
            {<<"from">>,    <<"monitorUser">>},
            {<<"type">>,    <<"reply">>},
            {<<"queryId">>, QueryId},
            {<<"lastPdu">>, true},
            {<<"value">>, {struct, [
                {<<"class">>, char_to_binary(Class)},
                {<<"reply">>, {json, Reply}}]}}
        ]
    }.


deleteTarget(TargetName) ->
    {struct,
        [
            {<<"from">>, <<"monitor_main">>},
            {<<"type">>, <<"deleteTarget">>},
            {<<"value">>, {struct, [
                {<<"name">>, char_to_binary(TargetName)}]}}
        ]
    }.

deleteProbe(Probe) ->
    #probe{name=Name,belong_to=Target} = Probe,
    {struct,
        [
            {<<"from">>, <<"monitor_main">>},
            {<<"type">>, <<"deleteProbe">>},
            {<<"value">>, {struct, [
                {<<"name">>,   char_to_binary(Name)},
                {<<"target">>, char_to_binary(Target)}]}}
        ]
    }.

probeReturn(ProbeReturn, Target, Probe, NextReturn) ->
    KeyValStr = make_key_values(ProbeReturn#probe_return.key_vals),
    JKeyVal = [{char_to_binary(Key), char_to_binary(Val)} || {Key, Val} <- KeyValStr],
    {struct,
        [
            {<<"from">>, <<"monitor_main">>},
            {<<"type">>, <<"probeReturn">>},
            {<<"value">>, {struct, [
                {<<"target">>,      char_to_binary(Target)},
                {<<"name">>,        char_to_binary(Probe)},
                {<<"status">>,      char_to_binary(ProbeReturn#probe_return.status)},
                {<<"replyString">>, char_to_binary(ProbeReturn#probe_return.reply_string)},
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
    JProp = [{char_to_binary(Key), maybe_str(Val)} || {Key,Val} <- Prop],
    {struct,
        [
            {<<"from">>, <<"monitor_main">>},
            {<<"type">>, <<"infoTarget">>},
            {<<"value">>, {struct, [
                {<<"name">>,          char_to_binary(Name)},
                {<<"properties">>,    {struct, JProp}},
                {<<"sysProperties">>, {struct, []}},
                {<<"infoType">>,      InfoType}]}
            }
        ]
    }.


maybe_str(Val) when is_integer(Val) -> Val;
maybe_str(Val) when is_float(Val) -> Val;
maybe_str(Val) -> char_to_binary(Val).

infoProbeCreate(Probe) -> infoProbe(Probe, <<"create">>).
infoProbeUpdate(Probe) -> infoProbe(Probe, <<"update">>).
infoProbe(Probe, InfoType) ->
    #probe{
        permissions   = #perm_conf{read = R, write = W},
        module_config = ProbeConf } = Probe,

    JR = [char_to_binary(G) || G <- R],
    JW = [char_to_binary(G) || G <- W],

    Identifier = ProbeConf#nchecks_probe_conf.identifier,

    {struct,
        [
            {<<"from">>, <<"monitor_main">>},
            {<<"type">>, <<"infoProbe">>},
            {<<"value">>, {struct, [
                {<<"target">>,   char_to_binary(Probe#probe.belong_to)},
                {<<"name">>,     char_to_binary(Probe#probe.name)},
                {<<"descr">>,    char_to_binary(Probe#probe.description)},
                {<<"perm">>,     {struct, [{<<"read">>, {array, JR}}, {<<"write">>, {array, JW}}]}},
                {<<"probeMod">>, atom_to_binary(Probe#probe.module, utf8)},
                {<<"probeId">>,  char_to_binary(Identifier)},
                {<<"status">>,   char_to_binary(Probe#probe.status)},
                {<<"timeout">>,  Probe#probe.timeout},
                {<<"step">>,     Probe#probe.step},
                {<<"active">>,   Probe#probe.active},
                {<<"infoType">>, InfoType}]}
            }
        ]
    }.


% UTILS

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

char_to_binary(V) ->
    unicode:characters_to_binary(V).

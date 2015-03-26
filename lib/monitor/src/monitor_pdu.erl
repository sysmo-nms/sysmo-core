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
    elementInterfaceReply/4,
    simpleReply/4,

    nchecksUpdateMessage/3,
    nchecksDumpMessage/3,
    loggerRrdEvent/3,
    loggerRrdDump/5
]).

loggerRrdDump(Target, Probe, Module, FilePaths, Dir) ->
    Indexes = [{I, list_to_binary(F)} || {I,F} <- FilePaths],
    {struct,
        [
            {<<"from">>, <<"monitor">>},
            {<<"type">>, <<"loggerRrdDump">>},
            {<<"value">>, {struct, [
                {<<"target">>,  list_to_binary(Target)},
                {<<"name">>,    list_to_binary(Probe)},
                {<<"logger">>,  atom_to_binary(Module, utf8)},
                {<<"path">>,    list_to_binary(Dir)},
                {<<"data">>,    false},
                {<<"indexes">>, {struct, Indexes}}]}
            }
        ]
    }.
                


loggerRrdEvent(Target, Probe, ClientUp) ->
    Updates = [{Index, list_to_binary(Up)} || {Index, Up} <- ClientUp],
    
    {struct,
        [
            {<<"from">>, <<"monitor">>},
            {<<"type">>, <<"loggerRrdEvent">>},
            {<<"value">>, {struct, [
                {<<"target">>, list_to_binary(Target)},
                {<<"name">>,  list_to_binary(Probe)},
                {<<"updates">>, {struct, Updates}}]}
            }
        ]
    }.

nchecksUpdateMessage(Probe, Ts, Updates) ->
    Up = [{list_to_binary(K), V} || {K,V} <- Updates],
    {struct,
        [
            {<<"from">>, <<"monitor">>},
            {<<"type">>, <<"nchecksUpdateMessage">>},
            {<<"value">>, 
                {struct, [
                    {<<"name">>,        list_to_binary(Probe)},
                    {<<"timestamp">>,   Ts},
                    {<<"rrdupdates">>,  {struct, Up}}
                ]}
            }
        ]
    }.

nchecksDumpMessage(Probe, DumpDir, RrdFile) ->
     {struct,
        [
            {<<"from">>, <<"monitor">>},
            {<<"type">>, <<"nchecksDumpMessage">>},
            {<<"value">>, 
                {struct, [
                    {<<"name">>,        list_to_binary(Probe)},
                    {<<"httpDumpDir">>, list_to_binary(DumpDir)},
                    {<<"rrdFile">>,     list_to_binary(RrdFile)}
                ]}
            }
        ]
    }.



elementInterfaceReply(QueryId, Status, Last, Info) ->
    {table, TableRows} = Info,
    IfTable = build_ifTable(TableRows, []),
    {struct,
        [
            {<<"from">>, <<"monitorUser">>},
            {<<"type">>, <<"elementInterfaceReply">>},
            {<<"value">>, {struct, [
                {<<"queryId">>, QueryId},
                {<<"status">>,  Status},
                {<<"last">>,    Last},
                {<<"ifInfo">>,  IfTable}]}}
        ]
    }.

build_ifTable([], Acc) -> {array, lists:reverse(Acc)};
build_ifTable([H|T], Acc) ->
    {table_row, IfIndex, IfDescr, IfType, IfMtu, IfSpeed, IfPhysAddress,
        IfAdminStatus, IfOperStatus, IfLastChange} = H,
    Elem = {struct, [
        {<<"ifIndex">>,         IfIndex},
        {<<"ifDescr">>,         list_to_binary(IfDescr)},
        {<<"ifType">>,          IfType},
        {<<"ifMTU">>,           IfMtu},
        {<<"ifSpeed">>,         IfSpeed},
        {<<"ifPhysAddress">>,   list_to_binary(IfPhysAddress)},
        {<<"ifAdminStatus">>,   IfAdminStatus},
        {<<"ifOperStatus">>,    IfOperStatus},
        {<<"ifLastChange">>,    list_to_binary(IfLastChange)}
    ]},
    build_ifTable(T, [Elem|Acc]).

    
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
        monitor_probe_conf  = ProbeConf } = Probe,
    
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
                {<<"info">>,        list_to_binary(Probe#probe.info)},
                {<<"perm">>,        {struct, [{<<"read">>, {array, JR}}, {<<"write">>, {array, JW}}]}},
                {<<"probeMod">>,    atom_to_binary(Probe#probe.monitor_probe_mod, utf8)},
                {<<"probeClass">>,  list_to_binary(Class)},
                {<<"status">>,      list_to_binary(Probe#probe.status)},
                {<<"timeout">>,     Probe#probe.timeout},
                {<<"step">>,        Probe#probe.step},
                {<<"inspectors">>,  gen_json_probe_inspectors(Probe#probe.inspectors)},
                {<<"loggers">>,     gen_json_probe_loggers(Probe#probe.loggers)},

                {<<"properties">>,  {struct, [{list_to_binary(Key), maybe_str(Val)} || {Key, Val} <- Probe#probe.properties]}},
                {<<"active">>,      Probe#probe.active},
                {<<"infoType">>,      InfoType}]}
            }
        ]
    }.

gen_json_probe_inspectors(Inspectors) ->
    {struct,
        [{atom_to_binary(Key, utf8), list_to_binary(io_lib:format("~p", [Conf]))} || {_,Key,Conf} <- Inspectors]
    }.
    
gen_json_probe_loggers([{logger, rrd_snmp_table_logger, Cfg}]) ->

    Type    = proplists:get_value(type, Cfg),
    RCreate = proplists:get_value(rrd_create, Cfg),
    RUpdate = proplists:get_value(rrd_update, Cfg),
    RGraphs = proplists:get_value(rrd_graph,  Cfg),
    RGraphs2 = [list_to_binary(G) || G <- RGraphs],
    Indexes = [I || {I,_} <- proplists:get_value(row_index_to_rrd_file, Cfg)],
    {struct,
        [{atom_to_binary(rrd_snmp_table_logger, utf8),
            {struct,
                [
                    {<<"type">>,        atom_to_binary(Type, utf8)},
                    {<<"rrdCreate">>,   list_to_binary(RCreate)},
                    {<<"rrdUpdate">>,   list_to_binary(RUpdate)},
                    {<<"rgraphs">>,     {array, RGraphs2}},
                    {<<"indexes">>,     {array, Indexes}}
                ]
            }
        }]
    };
gen_json_probe_loggers(A) ->
    io:format("what what waht ~p ~n", [A]),
    {struct, []}.
            

% UTILS
gen_str_probe_conf(Conf) when is_record(Conf, nchecks_probe_conf) ->
    lists:flatten(io_lib:format("~p", [Conf]));
gen_str_probe_conf(Conf) when is_record(Conf, snmp_probe_conf) ->
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

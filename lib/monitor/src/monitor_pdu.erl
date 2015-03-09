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
    simpleReply/4

]).


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
        {<<"ifIndex">>, IfIndex},
        {<<"ifDescr">>, list_to_binary(IfDescr)},
        {<<"ifType">>,  IfType},
        {<<"ifMTU">>,   IfMtu},
        {<<"ifSpeed">>, IfSpeed},
        {<<"ifPhysAddress">>, list_to_binary(IfPhysAddress)},
        {<<"ifAdminStatus">>, IfAdminStatus},
        {<<"ifOperStatus">>,  IfOperStatus},
        {<<"ifLastChange">>,  list_to_binary(IfLastChange)}
    ]},
    build_ifTable(T, [Elem|Acc]).

    
simpleReply(QueryId, Status, Last, Msg) ->
    {struct,
        [
            {<<"from">>, <<"monitorUser">>},
            {<<"type">>, <<"reply">>},
            {<<"value">>, {struct, [
                {<<"queryId">>, QueryId},   
                {<<"status">>, Status},
                {<<"last">>, Last},
                {<<"reply">>, list_to_binary(Msg)}]}}
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
                {<<"target">>, list_to_binary(Target)},
                {<<"name">>,  list_to_binary(Probe)},
                {<<"status">>, list_to_binary(ProbeReturn#probe_return.status)},
                {<<"originalRep">>, list_to_binary(ProbeReturn#probe_return.original_reply)},
                {<<"timestamp">>, ProbeReturn#probe_return.timestamp},
                {<<"keyVals">>,  {struct, JKeyVal}},
                {<<"nextReturn">>, NextReturn}
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
    

% 'PDU-MonitorPDU-fromServer-infoProbe-update'(
%     #probe{
%         permissions         = #perm_conf{read = R, write = W},
%         monitor_probe_conf  = ProbeConf,
%         description         = Descr,
%         info                = Info
%     } = Probe) ->
%     {modMonitorPDU,
%         {fromServer,
%             {infoProbe,
%                 {'InfoProbe',
%                     Probe#probe.belong_to,
%                     Probe#probe.name,
%                     Descr,
%                     Info,
%                     {'PermConf', R, W},
%                     atom_to_list(Probe#probe.monitor_probe_mod),
%                     gen_str_probe_conf(ProbeConf),
%                     Probe#probe.status,
%                     Probe#probe.timeout,
%                     Probe#probe.step,
%                     gen_asn_probe_inspectors(Probe#probe.inspectors),
%                     gen_asn_probe_loggers(Probe#probe.loggers),
%                     make_key_values(Probe#probe.properties),
%                     gen_asn_probe_active(Probe#probe.active),
%                     create
%                 }
%             }
%         }
%     }.


infoProbeCreate(Probe) -> infoProbe(Probe, <<"create">>).
infoProbeUpdate(Probe) -> infoProbe(Probe, <<"update">>).
infoProbe(Probe, InfoType) ->
    #probe{
        permissions         = #perm_conf{read = R, write = W},
        monitor_probe_conf  = ProbeConf } = Probe,
    
    JR = [list_to_binary(G) || G <- R],
    JW = [list_to_binary(G) || G <- W],
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
                {<<"probeconf">>,   list_to_binary(gen_str_probe_conf(ProbeConf))},
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
    
gen_json_probe_loggers([{logger, bmonitor_logger_rrd2, Cfg}]) ->
    Type    = proplists:get_value(type, Cfg),
    RCreate = proplists:get_value(rrd_create, Cfg),
    RUpdate = proplists:get_value(rrd_update, Cfg),
    RGraphs = proplists:get_value(rrd_graph, Cfg),
    RGraphs2 = [list_to_binary(G) || G <- RGraphs],
    Indexes = [I || {I,_} <- proplists:get_value(row_index_to_rrd_file, Cfg)],
    {struct,
        [{atom_to_binary(bmonitor_logger_rrd2, utf8),
            {struct,
                [
                    {<<"type">>,        list_to_binary(Type)},
                    {<<"rrdCreate">>,   list_to_binary(RCreate)},
                    {<<"RUpdate">>,     list_to_binary(RUpdate)},
                    {<<"RGraphs">>,     {array, RGraphs2}},
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

% gen_asn_probe_inspectors(Inspectors) ->
%     [{
%         'Inspector',
%         atom_to_list(Module),
%         lists:flatten(io_lib:format("~p", [Conf]))
%     } || {_, Module, Conf} <- Inspectors].
% 
% gen_asn_probe_loggers(Loggers) ->
%     [gen_logger_pdu(LConf) || LConf <- Loggers].
% 
% gen_logger_pdu({logger, bmonitor_logger_rrd2, Cfg}) ->
%     Type = proplists:get_value(type, Cfg),
%     RCreate = proplists:get_value(rrd_create, Cfg),
%     RUpdate = proplists:get_value(rrd_update, Cfg),
%     RGraphs = proplists:get_value(rrd_graph, Cfg),
%     Indexes = [I || {I,_} <- proplists:get_value(row_index_to_rrd_file, Cfg)],
%     {loggerRrd2, 
%         {'LoggerRrd2',
%             atom_to_list(bmonitor_logger_rrd2),
%             atom_to_list(Type),
%             RCreate,
%             RUpdate,
%             RGraphs,
%             Indexes
%         }
%     }.

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


%gen_asn_probe_active(true)  -> 1;
%gen_asn_probe_active(false) -> 0.

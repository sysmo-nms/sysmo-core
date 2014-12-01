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
    'PDU-MonitorPDU-fromServer-infoTarget-create'/1,
    'PDU-MonitorPDU-fromServer-infoProbe-create'/1,
    'PDU-MonitorPDU-fromServer-infoProbe-update'/1
]).

'PDU-MonitorPDU-fromServer-infoTarget-create'(
        #target{name=Name, properties=Prop}
    ) ->
    AsnProps = lists:foldl(fun({K,V}, Acc) -> 
        [{'Property', K, V} | Acc]
    end, [], Prop),
    {modMonitorPDU,
        {fromServer,
            {infoTarget,
                {'InfoTarget',
                    Name,
                    AsnProps,
                    [],
                    create}}}}.

'PDU-MonitorPDU-fromServer-infoProbe-update'(
    #probe{
        permissions         = #perm_conf{read = R, write = W},
        monitor_probe_conf  = ProbeConf,
        description         = Descr,
        info                = Info
    } = Probe) ->
    {modMonitorPDU,
        {fromServer,
            {infoProbe,
                {'InfoProbe',
                    Probe#probe.belong_to,
                    Probe#probe.name,
                    Descr,
                    Info,
                    {'PermConf', R, W},
                    atom_to_list(Probe#probe.monitor_probe_mod),
                    gen_asn_probe_conf(ProbeConf),
                    Probe#probe.status,
                    Probe#probe.timeout,
                    Probe#probe.step,
                    gen_asn_probe_inspectors(Probe#probe.inspectors),
                    gen_asn_probe_loggers(Probe#probe.loggers),
                    gen_asn_probe_properties(Probe#probe.properties),
                    gen_asn_probe_active(Probe#probe.active),
                    create
    }   }   }   }.



'PDU-MonitorPDU-fromServer-infoProbe-create'(
    #probe{
        permissions         = #perm_conf{read = R, write = W},
        monitor_probe_conf  = ProbeConf,
        description         = Descr,
        info                = Info
    } = Probe) ->
    {modMonitorPDU,
        {fromServer,
            {infoProbe,
                {'InfoProbe',
                    Probe#probe.belong_to,
                    Probe#probe.name,
                    Descr,
                    Info,
                    {'PermConf', R, W},
                    atom_to_list(Probe#probe.monitor_probe_mod),
                    gen_asn_probe_conf(ProbeConf),
                    Probe#probe.status,
                    Probe#probe.timeout,
                    Probe#probe.step,
                    gen_asn_probe_inspectors(Probe#probe.inspectors),
                    gen_asn_probe_loggers(Probe#probe.loggers),
                    gen_asn_probe_properties(Probe#probe.properties),
                    gen_asn_probe_active(Probe#probe.active),
                    create
    }   }   }   }.

% UTILS
gen_asn_probe_conf(Conf) when is_record(Conf, nchecks_probe_conf) ->
    lists:flatten(io_lib:format("~p", [Conf]));
gen_asn_probe_conf(Conf) when is_record(Conf, snmp_probe_conf) ->
    lists:flatten(io_lib:format("~p", [Conf])).

gen_asn_probe_inspectors(Inspectors) ->
    [{
        'Inspector',
        atom_to_list(Module),
        lists:flatten(io_lib:format("~p", [Conf]))
    } || {_, Module, Conf} <- Inspectors].

gen_asn_probe_loggers(Loggers) ->
    [gen_logger_pdu(LConf) || LConf <- Loggers].

gen_logger_pdu({logger, bmonitor_logger_rrd2, Cfg}) ->
    Type = proplists:get_value(type, Cfg),
    RCreate = proplists:get_value(rrd_create, Cfg),
    RUpdate = proplists:get_value(rrd_update, Cfg),
    RGraphs = proplists:get_value(rrd_graph, Cfg),
    Indexes = [I || {I,_} <- proplists:get_value(row_index_to_rrd_file, Cfg)],
    {loggerRrd2, 
        {'LoggerRrd2',
            atom_to_list(bmonitor_logger_rrd2),
            atom_to_list(Type),
            RCreate,
            RUpdate,
            RGraphs,
            Indexes
        }
    }.

gen_asn_probe_properties(K) ->
    gen_asn_probe_properties(K, []).
gen_asn_probe_properties([], S) ->
    S;
gen_asn_probe_properties([{K,V} | T], S) when is_list(V) ->
    gen_asn_probe_properties(T, [{'Property', K, V} | S]);
gen_asn_probe_properties([{K,V} | T], S) when is_integer(V) ->
    gen_asn_probe_properties(T, [{'Property', K, integer_to_list(V)} | S]);
gen_asn_probe_properties([{K,V} | T], S) when is_float(V) ->
    gen_asn_probe_properties(T, [{'Property', K, float_to_list(V, [{decimals, 10}])} | S]);
gen_asn_probe_properties([{K,V} | T], S) when is_atom(V) ->
    gen_asn_probe_properties(T, [{'Property', K, atom_to_list(V)} | S]).


gen_asn_probe_active(true)  -> 1;
gen_asn_probe_active(false) -> 0.

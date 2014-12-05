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
% @private
-module(monitor).
-include("include/monitor.hrl").
-include("include/monitor_snmp.hrl").
-include("../equartz/include/equartz.hrl").
-export([
    target_new/2,
    target_delete/1,
    job_new/2,
    job_delete/1,
    probe_new/2,
    probe_delete/1,

    which_targets/0,
    which_probes/0,
    which_jobs/0,

    fill_test/1
]).

-define(RRD_ifPerf_file, "snmp_if_perf.ini").

which_targets() -> monitor_data_master:which(target).
which_probes()  -> monitor_data_master:which(probe).
which_jobs()    -> monitor_data_master:which(job).

target_delete(TargetName) ->
    monitor_data_master:delete(target,TargetName).

probe_delete(ProbeName) ->
    monitor_data_master:delete(probe,ProbeName).

job_delete(JobName) ->
    monitor_data_master:delete(job,JobName).

fill_test(0) -> ok;
fill_test(N) ->
    SysProp = [
        {snmp_port,     161},
        {snmp_version,  "2c"},
        {snmp_seclevel, "noAuthNoPriv"},
        {snmp_community,"public"},
        {snmp_usm_user, "undefined"},
        {snmp_authkey,  "undefined"},
        {snmp_authproto,"MD5"},
        {snmp_privkey,  "undefined"},
        {snmp_privproto,"DES"},
        {snmp_timeout,  5000},
        {snmp_retries,  1}
    ],
    Prop = [
        {"ip",          "192.168.0.5"},
        {"ipVersion",   "v4"},
        {"dnsName",     "undefined"},
        {"sysName",     "undefined"}
    ],

    K = target_new(SysProp, Prop),
    probe_new({nchecks, icmp},    K),
    probe_new({snmp, walk_table}, K),
    job_new({internal, update_snmp_system_info}, K),
    job_new({internal, update_snmp_if_aliases},  K),
    fill_test(N - 1).

%%-----------------------------------------------------------------------------
%% TARGET API
%%-----------------------------------------------------------------------------
target_new(SysProp, []) ->
    Prop = [{"ip", "none"}, {"sysName", "undefined"}, {"dnsName", "undefined"}],
    target_new(SysProp, Prop);
target_new(SysProp, Prop) ->
    T = #target{sys_properties=SysProp,properties=Prop},
    monitor_data_master:new(target, T).


%%-----------------------------------------------------------------------------
%% JOB API
%%-----------------------------------------------------------------------------
job_new({internal, Function}, Target) ->
    J = #job{
        belong_to = Target,
        trigger  = ?CRON_EVERY20S,
        %trigger  = ?CRON_DAILY4AM,
        module   = monitor_jobs,
        function = Function,
        argument = Target,
        info     = lists:concat([?CRON_EVERY20S, " ", monitor_jobs, " ", Function, " ", Target])
    },
    monitor_data_master:new(job, J).

%%-----------------------------------------------------------------------------
%% PROBE API
%%-----------------------------------------------------------------------------
probe_new({nchecks, icmp}, Target) ->
    Probe = #probe{
        belong_to   = Target,
        description = "ICMP:Echo presence",
        monitor_probe_mod = bmonitor_probe_nchecks,
        monitor_probe_conf = #nchecks_probe_conf{
            function    = icmp,
            args        = []
        },
        inspectors  = [
            #inspector{
                module   = bmonitor_inspector_status_set,
                conf     = []
            }
        ]
    },
    monitor_data_master:new(probe, Probe);

probe_new({snmp, walk_table}, Target) ->
    {RrdCreate, RrdUpdate, RrdGraphs} = get_rrd_template(),
    Probe = #probe{
        belong_to   = Target,
        description = "SNMP:Interfaces performances",
        monitor_probe_mod = bmonitor_probe_snmp,



        % TEMPLATE BEGIN
        % This is a ifperf probe. This must be defined in a template file.
        monitor_probe_conf = #snmp_probe_conf{
            method      = walk_table,
            retries     = 1,
            oids        = [
                ?IF_INDEX,
                ?IF_DESCR,
                ?IF_IN_OCTETS,
                ?IF_IN_UCASTPKTS,
                ?IF_IN_NUCASTPKTS,
                ?IF_IN_ERRORS,
                ?IF_OUT_OCTETS,
                ?IF_OUT_UCASTPKTS,
                ?IF_OUT_NUCASTPKTS,
                ?IF_OUT_ERRORS
            ]
        },

        loggers = [
            #logger{
                module = bmonitor_logger_rrd2, 
                conf = [
                    {type,                  snmp_table},
                    {rrd_create,            RrdCreate},
                    {row_index_to_rrd_file,
                        [
                            {1, "index1.rrd"},
                            {2, "index2.rrd"},
                            {3, "index3.rrd"}
                        ]
                    },
                    {rrd_update,            RrdUpdate},
                    {rrd_graph,             RrdGraphs},
                    {row_index_pos_to_rrd_template,
                        % as defined in walk_table method 
                        % (+ 1 for the atom table_row)
                        [
                            4, %?IF_IN_OCTETS
                            8, %?IF_OUT_OCTETS
                            5, %?IF_IN_UCASTPKTS
                            6, %?IF_IN_NUCASTPKTS
                            7, %?IF_IN_ERRORS
                            9, %?IF_OUT_UCASTPKTS
                            10,%?IF_OUT_NUCASTPKTS
                            11 %?IF_OUT_ERRORS
                        ]
                    }
                ]
            }
        ],
        % TEMPLATE END




        inspectors = [
            #inspector{
                module = bmonitor_inspector_status_set, 
                conf   = []
            }
        ]
    },
    monitor_data_master:new(probe, Probe).

get_rrd_template() ->
    %?RRD_ifPerf_file
    {ok, R} = application:get_env(monitor, rrd_templates_dir),
    TplFile = filename:join(R, ?RRD_ifPerf_file),
    {ok, IniBin} = file:read_file(TplFile),
    IniStr = erlang:binary_to_list(IniBin),
    {ok, IniVal} = ini:parse_string(IniStr),

    CreatePart = proplists:get_value(create, IniVal),
    CreateComm = proplists:get_value(command, CreatePart),

    UpdatePart = proplists:get_value(update, IniVal),
    UpdateComm = proplists:get_value(command, UpdatePart),

    GraphPart  = proplists:get_value(graph,  IniVal),
    GraphComm  = [Graph || {graph, Graph} <- GraphPart],

    {CreateComm,UpdateComm,GraphComm}.

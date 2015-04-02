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
    job_fire/1,
    job_delete/1,
    probe_new/2,
    probe_delete/1,

    which_targets/0,
    which_probes/0,
    which_jobs/0,

    fill_test/1,

    probe_trigger_return/2,
    probe_shutdown/1,
    probe_force/1
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

fill_test(N) ->
    fill_test(N, "self").
fill_test(0,_) -> ok;
fill_test(N,Parent) ->
    SysProp = [
        {"snmp_port",     161},
        {"snmp_version",  "2c"},
        {"snmp_seclevel", "noAuthNoPriv"},
        {"snmp_community","public"},
        {"snmp_usm_user", "undefined"},
        {"snmp_authkey",  "undefined"},
        {"snmp_authproto","MD5"},
        {"snmp_privkey",  "undefined"},
        {"snmp_privproto","DES"},
        {"snmp_timeout",  5000},
        {"snmp_retries",  1}
    ],
    Prop = [
        {"host",          "192.168.0.5"},
        {"dnsName",     "undefined"},
        {"sysName",     "undefined"}
    ],

    K = target_new(SysProp, Prop),
    Ping = probe_new({nchecks, "CheckICMP", []}, K),
    Snmp = probe_new({snmp, if_perfs, [1,2,3]}, K),
    dependency_new(Ping, Parent),
    dependency_new(Snmp, Parent),
    job_new({internal, update_snmp_system_info}, K),
    job_new({internal, update_snmp_if_aliases},  K),
    fill_test(N - 1, Ping).
%%-----------------------------------------------------------------------------
%% TARGET API
%%-----------------------------------------------------------------------------
target_new(SysProp, Props) ->
    Default = ?DEFAULT_TARGET_PROPERTIES,
    NewProp = lists:foldl(fun({K,V},Acc) -> lists:keystore(K,1,Acc,{K,V}) end, Default, Props),
    T = #target{sys_properties=SysProp,properties=NewProp},
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
        info     = lists:concat([?CRON_EVERY20S, ",", monitor_jobs, ",", Function, ",", Target])
    },
    monitor_data_master:new(job, J).


job_fire(JobId) ->
    equartz:fire_now(JobId).

%%-----------------------------------------------------------------------------
%% PROBE API
%%-----------------------------------------------------------------------------
dependency_new(Probe, Depend) ->
    monitor_data_master:new(dependency, #dependency{a_probe=Probe,his_parent=Depend}).

%%-----------------------------------------------------------------------------
%% PROBE API
%%-----------------------------------------------------------------------------
probe_new({nchecks, JavaClass, Args}, Target) ->
    Probe = #probe{
        belong_to   = Target,
        description = lists:concat(["NCHECKS: ", JavaClass]),
        module = probe_nchecks,
        module_config = #nchecks_probe_conf{
            class       = JavaClass,
            args        = Args
        }
    },
    monitor_data_master:new(probe, Probe);

probe_new({snmp, if_perfs, Indexes}, Target) ->
    IndexesFiles = [{Index,lists:concat(["index",Index,".rrd"])} || Index <- Indexes],
    io:format("indexes: ~p~n",[IndexesFiles]),
    Probe = #probe{
        belong_to   = Target,
        description = "SNMP:Interfaces performances",
        module = snmp_ifPerf,
        module_config = IndexesFiles
    },
    monitor_data_master:new(probe, Probe).


-spec probe_trigger_return(PidName::string(), CState::#client_state{}) -> ok.
% @private
% @doc
% Used by the monitor main channel to initialize clients. This function send
% a Partial Probe return PDU to the specified client, including the next expected
% return time.
% @end
probe_trigger_return(PidName, CState) ->
    gen_server:cast({via, supercast_registrar, PidName}, {trigger_return, CState}).

-spec probe_shutdown(PidName::string()) -> ok.
% @private
% @doc
% Used by monitor datamaster. It shut down the probe specified wile data master
% delete it from the db.
% @end
probe_shutdown(PidName) ->
    case supercast_registrar:whereis_name(PidName) of
        undefined ->
            ok;
        Pid ->
            gen_server:call(Pid, shut_it_down)
    end.

-spec probe_force(PidName::string()) -> ok.
% @private
% @doc
% Force a probe check as soon as possible. Used mainly from monitor API.
% @end
probe_force(PidName) ->
    case supercast_registrar:whereis_name(PidName) of
        undefined ->
            ok;
        Pid ->
            gen_server:cast(Pid, force)
    end.

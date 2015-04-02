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
    % public
    new_target/2,
    new_job/2,
    new_probe/2,

    del_target/1,
    del_job/1,
    del_probe/1,

    fire_job/1,

    which_targets/0,
    which_probes/0,
    which_jobs/0,

    force_probe/1,

    trigger_probe_return/2,

    % private probe utils 
    timestamp/0,
    read_timer/1,
    generate_temp_dir/0,
    send_after/2,
    send_after_rand/2,

    
    % tests
    fill_test/1

]).


%%-----------------------------------------------------------------------------
%% PUBLIC API
%%-----------------------------------------------------------------------------
%% TODO python access to these functions
which_targets() -> monitor_data_master:which(target).
which_probes()  -> monitor_data_master:which(probe).
which_jobs()    -> monitor_data_master:which(job).

del_target(TargetName) ->
    monitor_data_master:delete(target,TargetName).

del_probe(ProbeName) ->
    monitor_data_master:delete(probe,ProbeName).

del_job(JobName) ->
    monitor_data_master:delete(job,JobName).

new_target(SysProp, Props) ->
    Default = ?DEFAULT_TARGET_PROPERTIES,
    NewProp = lists:foldl(fun({K,V},Acc) -> lists:keystore(K,1,Acc,{K,V}) end, Default, Props),
    T = #target{sys_properties=SysProp,properties=NewProp},
    monitor_data_master:new(target, T).

new_job({internal, Function}, Target) ->
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


fire_job(JobId) ->
    equartz:fire_now(JobId).

dependency_new(Probe, Depend) ->
    monitor_data_master:new(dependency, #dependency{a_probe=Probe,his_parent=Depend}).

new_probe({probe_nchecks, JavaClass, Args}, Target) ->
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

new_probe({snmp_ifPerf, Indexes}, Target) ->
    IndexesFiles = [{Index,lists:concat(["index",Index,".rrd"])} || Index <- Indexes],
    io:format("indexes: ~p~n",[IndexesFiles]),
    Probe = #probe{
        belong_to   = Target,
        description = "SNMP:Interfaces performances",
        module = snmp_ifPerf,
        module_config = IndexesFiles
    },
    monitor_data_master:new(probe, Probe).

force_probe(PidName) ->
    case supercast_registrar:whereis_name(PidName) of
        undefined ->
            ok;
        Pid ->
            gen_server:cast(Pid, force)
    end.
%%-----------------------------------------------------------------------------
%% PUBLIC API END
%%-----------------------------------------------------------------------------






%%-----------------------------------------------------------------------------
%% PRIVATE API
%%-----------------------------------------------------------------------------
-spec trigger_probe_return(PidName::string(), CState::#client_state{}) -> ok.
% @private
% @doc
% Used by the monitor main channel to initialize clients. This function send
% a Partial Probe return PDU to the specified client, including the next expected
% return time. It did not trigger a check.
% @end
trigger_probe_return(PidName, CState) ->
    gen_server:cast({via, supercast_registrar, PidName}, {trigger_return, CState}).

-spec timestamp() -> {Seconds::integer(), Microseconds::integer()}.
% @private
% @doc
% Used by the probes. Return timestamp in seconds and microseconds.
% @end
timestamp() ->
    {Meg,Sec,Micro} = os:timestamp(),
    Seconds = Meg * 1000000 + Sec,
    MicroSeconds = Seconds * 1000000 + Micro,
    {Seconds, MicroSeconds}.

-spec read_timer(TimerReference::tuple()) -> Microseconds::integer().
% @private
% @doc
% Do erlang:read_timer(Tref) but return 0 if read_timer return false.
% Used by the probes.
% @end
read_timer(TRef) ->
    case erlang:read_timer(TRef) of
        false -> 0;
        Any -> Any
    end.

-spec generate_temp_dir() -> TmpdirString::string().
% @private
% @doc
% The string returned can be used to create a temporary directory under dump.
% Used by the probes for synchronization events.
% @end
generate_temp_dir() ->
    {_,Sec,Micro} = os:timestamp(),
    MicroSec = Sec * 1000000 + Micro,
    lists:concat(["tmp-", MicroSec]).

-spec send_after(Step::integer(), Msg::any()) -> TRef::tuple().
% @private
% @doc
% send Msg after Step seconds.
% Used by probes.
% @end
send_after(Step, Msg) ->
    erlang:send_after(Step * 1000, self(), Msg).

-spec send_after_rand(Step::integer(), Msg::any()) -> TRef::tuple().
% @private
% @doc
% send Msg after random time between 0 and Step seconds.
% Used by probes.
% @end
send_after_rand(Step, Msg) ->
    send_after(random:uniform(Step), Msg).







% tests
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

    K = new_target(SysProp, Prop),
    Ping = new_probe({nchecks, "CheckICMP", []}, K),
    Snmp = new_probe({snmp, if_perfs, [1,2,3]}, K),
    dependency_new(Ping, Parent),
    dependency_new(Snmp, Parent),
    new_job({internal, update_snmp_system_info}, K),
    new_job({internal, update_snmp_if_aliases},  K),
    fill_test(N - 1, Ping).

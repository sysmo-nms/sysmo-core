%%==============================================================================
%% Sysmo NMS Network Management and Monitoring solution (https://sysmo-nms.github.io)
%%
%% Copyright (c) 2012-2017 Sebastien Serre <ssbx@sysmo.io>
%%
%% Sysmo NMS is free software: you can redistribute it and/or modify it under
%% the terms of the GNU General Public License as published by the Free Software
%% Foundation, either version 3 of the License, or (at your option) any later
%% version.
%%
%% Sysmo NMS is distributed in the hope that it will be useful, but WITHOUT ANY
%% WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
%% A PARTICULAR PURPOSE. See the GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License along with
%% Sysmo.  If not, see <http://www.gnu.org/licenses/>.
%%==============================================================================
% @private
-module(monitor).
-include("monitor.hrl").
-include_lib("common_hrl/include/logs.hrl").

-export([new_target/2, new_job/2, new_probe/2]).
-export([del_target/1, del_job/1, del_probe/1]).
-export([fire_job/1, force_probe/1, trigger_nchecks_reply/2]).
-export([which_targets/0, which_probes/0, which_jobs/0]).

% private probe utils
-export([timestamp/0, read_timer/1, generate_temp_dir/0,
    send_after/2, send_after_rand/2]).

% tests
-export([fill_test/1]).

%% pid registry behaviour
-export([
    register_name/2,
    unregister_name/1,
    whereis_name/1,
    send/2]).

%%------------------------------------------------------------------------------
%% PUBLIC API
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @see monitor_data_master:which/1
%% @doc
%% Return all known targets identifiers.
%% @end
%%------------------------------------------------------------------------------
-spec(which_targets() -> Targets :: [string()]).
which_targets() -> monitor_data_master:which(target).

%%------------------------------------------------------------------------------
%% @see monitor_data_master:which/1
%% @doc
%% Return all known probes identifiers.
%% @end
%%------------------------------------------------------------------------------
-spec(which_probes() -> Probes :: [string()]).
which_probes()  -> monitor_data_master:which(probe).

%%------------------------------------------------------------------------------
%% @see monitor_data_master:which/1
%% @doc
%% Return all known jobs identifiers.
%% @end
%%------------------------------------------------------------------------------
-spec(which_jobs() -> Jobs :: [string()]).
which_jobs()    -> monitor_data_master:which(job).

%%------------------------------------------------------------------------------
%% @doc
%% Delete the target by his ID. Will delete the target probes and jobs.
%% @end
%%------------------------------------------------------------------------------
-spec(del_target(TargetId :: string()) -> ok | abort).
del_target(TargetName) ->
    monitor_data_master:delete(target,TargetName).

%%------------------------------------------------------------------------------
%% @doc
%% Delete the target by his ID.
%% @end
%%------------------------------------------------------------------------------
-spec(del_probe(ProbeId :: string()) -> ok | abort).
del_probe(ProbeName) ->
    monitor_data_master:delete(probe,ProbeName).

%%------------------------------------------------------------------------------
%% @doc
%% Delete the job by his ID.
%% @end
%%------------------------------------------------------------------------------
-spec(del_job(JobId :: string()) -> ok | abort).
del_job(JobName) ->
    monitor_data_master:delete(job,JobName).

new_target(SysProp, Props) ->
    Default = ?DEFAULT_TARGET_PROPERTIES,
    NewProp = lists:foldl(
        fun({K,V},Acc) ->
           lists:keystore(K,1,Acc,{K,V})
        end,
    Default, Props),
    T = #target{sys_properties=SysProp,properties=NewProp},
    monitor_data_master:new(target, T).

new_job(Function, Target) ->
    J = #job{
        belong_to = Target,
        trigger  = ?CRON_EVERYHOURS,
        module   = monitor_jobs,
        function = Function,
        argument = Target,
        info     = lists:concat([?CRON_EVERYHOURS, ",", monitor_jobs, ",", Function, ",", Target])
    },
    monitor_data_master:new(job, J).


fire_job(JobId) ->
    case (catch monitor_scheduler:fire_now(JobId)) of
        {_ERROR, _} = Err -> % _ERROR = 'EXIT' | timeout
            ?LOG_ERROR("fire_job failure", Err);
        ok -> ok
    end.

dependency_new(Probe, Depend) ->
    monitor_data_master:new(dependency, #dependency{a_probe=Probe,his_parent=Depend}).

new_probe({nchecks_probe, Identifier, JavaClass, Display, Args}, Target) ->
    % TODO if !(JavaClass.isSnmp && Target.isSnmp) return error;
    Probe = #probe{
        belong_to   = Target,
        description = Display,
        module = nchecks_probe,
        module_config = #nchecks_probe_conf{
            identifier  = Identifier,
            class       = JavaClass,
            args        = Args
        }
    },
    monitor_data_master:new(probe, Probe).

force_probe(PidName) ->
    nchecks_probe:force(PidName).

%%-----------------------------------------------------------------------------
%% PUBLIC API END
%%-----------------------------------------------------------------------------






%%-----------------------------------------------------------------------------
%% PRIVATE API
%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% Used by the monitor main channel to initialize clients. This function send
%% a Partial Probe return PDU to the specified client, including the next expected
%% return time. It did not trigger a check.
%% @end
%%-----------------------------------------------------------------------------
-spec trigger_nchecks_reply(PidName::string(), CState :: tuple()) -> ok.
trigger_nchecks_reply(PidName, CState) ->
    nchecks_probe:trigger_return(PidName, CState).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% Used by the probes. Return timestamp in seconds and microseconds.
%% @end
%%-----------------------------------------------------------------------------
-spec timestamp() -> {Seconds::integer(), Microseconds::integer()}.
timestamp() ->
    {Meg,Sec,Micro} = os:timestamp(),
    Seconds = Meg * 1000000 + Sec,
    MicroSeconds = Seconds * 1000000 + Micro,
    {Seconds, MicroSeconds}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% Do erlang:read_timer(Tref) but return 0 if read_timer return false.
%% Used by the probes.
%% @end
%%-----------------------------------------------------------------------------
-spec read_timer(TimerReference::tuple()) -> Microseconds::integer().
read_timer(TRef) ->
    case erlang:read_timer(TRef) of
        false -> 0;
        Any -> Any
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @TODO should be a supercast work
%% @doc
%% The string returned can be used to create a temporary directory under dump.
%% Used by the probes for synchronization events.
%% @end
%%-----------------------------------------------------------------------------
-spec generate_temp_dir() -> TmpdirString::string().
generate_temp_dir() ->
    {_,Sec,Micro} = os:timestamp(),
    MicroSec = Sec * 1000000 + Micro,
    lists:concat(["tmp-", MicroSec]).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% send Msg after Step seconds.
%% Used by probes.
%% @end
%%-----------------------------------------------------------------------------
-spec send_after(Step::integer(), Msg::any()) -> TRef::tuple().
send_after(Step, Msg) ->
    erlang:send_after(Step * 1000, self(), Msg).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% send Msg after random time between 0 and Step seconds.
%% Used by probes.
%% @end
%%-----------------------------------------------------------------------------
-spec send_after_rand(Step::integer(), Msg::any()) -> TRef::tuple().
send_after_rand(Step, Msg) ->
    send_after(rand:uniform(Step), Msg).



%%%=============================================================================
%%% Pid registry behaviour callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @see global:register_name/2
%% @doc
%% Same behaviour has global:register_name/2 with strings as names.
%%
%% @end
%%------------------------------------------------------------------------------
-spec register_name(Name :: string(), Pid :: pid()) -> yes | no.
register_name(Name, Pid) ->
    case where(Name) of
        undefined ->
            true = ets:insert(?ETS_PROBES_REGISTER, {Name, Pid}),
            yes;
        _ ->
            no
    end.

%%------------------------------------------------------------------------------
%% @private
%% @see global:unregister_name/1
%% @doc
%% Same behaviour has global:unregister_name/1 but with strings as names.
%%
%% @end
%%------------------------------------------------------------------------------
-spec unregister_name(Name::string()) -> Name::string().
unregister_name(Name) ->
    true = ets:delete(?ETS_PROBES_REGISTER, Name),
    Name.

%%------------------------------------------------------------------------------
%% @private
%% @see global:whereis_name/1
%% @doc
%% Same behaviour has global:whereis_name/1 but with strings as names.
%%
%% @end
%%------------------------------------------------------------------------------
-spec whereis_name(Name::string()) -> pid() | undefined.
whereis_name(Name) -> where(Name).
where(Name) ->
    case ets:lookup(?ETS_PROBES_REGISTER, Name) of
        [{Name,Pid}] ->
            case is_process_alive(Pid) of
                true  -> Pid;
                false -> undefined
            end;
        [] -> undefined
    end.

%%------------------------------------------------------------------------------
%% @private
%% @see global:send/2
%% @doc
%% Same behaviour has global:send/2 but with strings as names.
%%
%% @end
%%------------------------------------------------------------------------------
-spec send(Name::string, Msg::term()) -> pid().
send(Name, Msg) ->
    case where(Name) of
        Pid when is_pid(Pid) ->
            Pid ! Msg,
            Pid;
        undefined ->
            exit({badarg, {Name, Msg}})
    end.




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
        {"host",        "192.168.0.5"},
        {"dnsName",     "undefined"},
        {"sysName",     "undefined"}
    ],

    K    = new_target(SysProp, Prop),
    Ping = new_probe({nchecks, "CheckICMP", []}, K),
    Snmp = new_probe({nchecks, "CheckNetworkInterfaces", [1,2,3]}, K),
    dependency_new(Ping, Parent),
    dependency_new(Snmp, Parent),
    new_job(update_snmp_system_info, K),
    new_job(update_snmp_if_aliases,  K),
    fill_test(N - 1, Ping).

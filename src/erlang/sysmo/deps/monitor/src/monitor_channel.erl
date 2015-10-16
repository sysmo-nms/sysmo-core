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
-module(monitor_channel).
-behaviour(gen_server).
-behaviour(supercast_channel).
-include("include/monitor.hrl").
-include_lib("common_hrl/include/logs.hrl").

% GEN_SERVER
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

% GEN_CHANNEL
-export([
    get_perms/1,
    sync_request/2
]).

% SRV
-export([
    start_link/0
]).

-record(state, {
    perm
}).



start_link() ->
    gen_server:start_link(
      {via, supercast_registrar, {?MODULE, ?MASTER_CHANNEL}}, ?MODULE, [], []).



%%----------------------------------------------------------------------------
%% supercast_channel API
%%----------------------------------------------------------------------------
-spec get_perms(PidName::atom()) -> {ok, PermConf::#perm_conf{}}.
get_perms(PidName) ->
    gen_server:call({via, supercast_registrar, PidName}, get_perms).

-spec sync_request(PidName::atom(), CState::tuple()) ->  ok.
sync_request(PidName, CState) ->
    gen_server:cast({via, supercast_registrar, PidName}, {sync_request, CState}).


%%----------------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%----------------------------------------------------------------------------
init([]) ->
    {ok, Read}  = application:get_env(monitor, master_chan_read_perm),
    {ok, Write} = application:get_env(monitor, master_chan_write_perm),
    mnesia:subscribe({table, target, detailed}),
    mnesia:subscribe({table, probe,  detailed}),
    mnesia:subscribe({table, job,    detailed}),
    mnesia:subscribe({table, dependency, detailed}),
    {ok, #state{perm=#perm_conf{read=Read,write=Write}}}.

%%----------------------------------------------------------------------------
%% SUPERCAST_CHANNEL BEHAVIOUR CALLS
%%----------------------------------------------------------------------------
handle_call(get_perms, _F, #state{perm = P} = S) ->
    {reply, P, S}.

%%----------------------------------------------------------------------------
%% SUPERCAST_CHANNEL BEHAVIOUR CASTS
%%----------------------------------------------------------------------------
handle_cast({sync_request, CState}, S) ->
    supercast_channel:subscribe(?MASTER_CHANNEL, CState),

    {ok, DumpDir} = application:get_env(supercast, http_sync_dir),
    TmpDir = monitor:generate_temp_dir(),
    DumpPath = filename:join(DumpDir,TmpDir),
    file:make_dir(DumpPath),
    {ok, LatestEventsFile} = eventdb:dump_latest_events(DumpPath),

    BeginPdu = monitor_pdu:masterSyncBegin(TmpDir,LatestEventsFile),
    supercast_channel:unicast(CState, [BeginPdu]),

    {ok, _} = monitor_data_master:iterate(target, fun(T,_) ->
        #target{permissions=Perm} = T,
        case supercast:satisfy(CState, Perm) of
            true    ->
                Pdu = monitor_pdu:infoTargetCreate(T),
                ok  = supercast_channel:unicast(CState, [Pdu]);
            false   -> ok
        end
    end),

    {ok, _} = monitor_data_master:iterate(probe, fun(P,_) ->
        #probe{name=_Name,permissions=Perm} = P,
        case supercast:satisfy(CState, Perm) of
            true    ->
                Pdu = monitor_pdu:infoProbeCreate(P),
                ok  = supercast_channel:unicast(CState, [Pdu]),
                % Dep = do_get(dependency,Name)
                % Pdu = dep... supercast:unicast...
                ?LOG_INFO("Should_send_dependencies"),
                monitor:trigger_nchecks_reply(P#probe.name, CState);
            false   -> ok
        end
    end),

    {ok, _Jobs} = monitor_data_master:iterate(job, fun(J,Acc) ->
        #job{permissions=Perm} = J,
        case supercast:satisfy(CState, Perm) of
            true    ->
                [J|Acc];
            false   ->
                Acc
        end
    end),

    ?LOG_INFO("Should_send_jobs", _Jobs),

    EndPdu = monitor_pdu:masterSyncEnd(),
    supercast_channel:unicast(CState, [EndPdu]),
    {noreply, S};

handle_cast(_R, S) ->
    {noreply, S}.

handle_info({mnesia_table_event, {write, target, Target, [], _ActivityId}}, S) ->
    handle_target_create(Target),
    {noreply, S};
handle_info({mnesia_table_event, {write, target, Target, [Target], _ActivityId}}, S) ->
    % same thing do nothing
    {noreply, S};
handle_info({mnesia_table_event, {write, target, NewTarget, OldTarget, _ActivityId}}, S) ->
    handle_target_update(NewTarget, OldTarget),
    {noreply, S};

handle_info({mnesia_table_event, {write, probe, Probe, [], _ActivityId}}, S) ->
    handle_probe_create(Probe),
    {noreply, S};
handle_info({mnesia_table_event, {write, probe, NewProbe, [OldProbe], _ActivityId}}, S) ->
    handle_probe_update(NewProbe, OldProbe),
    {noreply, S};

handle_info({mnesia_table_event, {write, job, Job, [], _ActivityId}}, S) ->
    handle_job_create(Job),
    {noreply, S};
handle_info({mnesia_table_event, {write, job, Job, [Job], _ActivityId}}, S) ->
    % same thing do nothing
    {noreply, S};
handle_info({mnesia_table_event, {write, job, NewJob, [OldJob], _ActivityId}}, S) ->
    handle_job_update(NewJob, OldJob),
    {noreply, S};


handle_info({mnesia_table_event, {write, dependency, Dep, [], _ActivityId}}, S) ->
    handle_dependency_create(Dep),
    {noreply, S};
handle_info({mnesia_table_event, {write, dependency, Dep, [_OldDep], _ActivityId}}, S) ->
    handle_dependency_update(Dep),
    {noreply, S};


handle_info({mnesia_table_event, {delete, _Table, What, [OldRecord], _ActivityId}}, S) ->
    handle_delete(What, OldRecord),
    {noreply, S};

handle_info(_I, S) ->
    ?LOG_WARNING({"Received handle info: ", _I}),
    {noreply, S}.

terminate(_R, _) ->
    normal.

code_change(_O, S, _E) ->
    {ok, S}.


% MNESIA EVENTS
handle_target_create(#target{permissions=Perm} = Target) ->
    Pdu = monitor_pdu:infoTargetCreate(Target),
    supercast_channel:emit(?MASTER_CHANNEL, {Perm, Pdu}).

handle_target_update(#target{permissions=Perm} = Target, _) ->
    Pdu = monitor_pdu:infoTargetUpdate(Target),
    supercast_channel:emit(?MASTER_CHANNEL, {Perm, Pdu}).


handle_probe_create(#probe{permissions=Perm} = Probe) ->
    Pdu = monitor_pdu:infoProbeCreate(Probe),
    supercast_channel:emit(?MASTER_CHANNEL, {Perm, Pdu}).

handle_probe_update(#probe{permissions=Perm} = Probe,_) ->
    Pdu = monitor_pdu:infoProbeUpdate(Probe),
    supercast_channel:emit(?MASTER_CHANNEL, {Perm, Pdu}).


handle_job_create(#job{permissions=_Perm} = _Job) ->
    ?LOG_INFO("Create job", _Job).

handle_job_update(#job{permissions=_Perm} = _Job, _) ->
    ?LOG_INFO("Update job", _Job).


handle_dependency_create(_Dep) ->
    ?LOG_INFO("Create dep", _Dep).
handle_dependency_update(_Dep) ->
    ?LOG_INFO("update dep", _Dep).


handle_delete({target, Name}, #target{permissions=Perm}) ->
    Pdu = monitor_pdu:deleteTarget(Name),
    supercast_channel:emit(?MASTER_CHANNEL, {Perm, Pdu});
handle_delete({probe, _}, #probe{permissions=Perm} = Probe) ->
    Pdu = monitor_pdu:deleteProbe(Probe),
    supercast_channel:emit(?MASTER_CHANNEL, {Perm, Pdu});
handle_delete({job, _Name},_) ->
    ?LOG_INFO("Delete job", _Name);
handle_delete({dependency, _Name},_) ->
    ?LOG_INFO("Delete dependency", _Name).

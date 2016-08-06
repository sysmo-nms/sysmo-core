%%==============================================================================
%% Sysmo NMS Network Management and Monitoring solution (http://www.sysmo.io)
%%
%% Copyright (c) 2012-2016 Sebastien Serre <ssbx@sysmo.io>
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
-module(monitor_channel).
-behaviour(gen_server).
-behaviour(supercast_proc).
-include("monitor.hrl").
-include_lib("common_hrl/include/logs.hrl").

% GEN_SERVER
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

% GEN_CHANNEL
-export([join_request/4, leave_request/4, info_request/3]).

% called from nchecks_probe
-export([send_unicast/2]).

% SRV
-export([start_link/0]).

-record(state, {perm}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send_unicast(CState, Pdu) ->
    gen_server:cast(?MODULE, {send_unicast, CState, Pdu}).

%%----------------------------------------------------------------------------
%% supercast API
%%----------------------------------------------------------------------------
join_request(_Channel, Args, CState, Ref) ->
    Self = Args,
    gen_server:cast(Self, {sync_request, CState, Ref}).

leave_request(_Channel, _Args = Self, _CState, Ref) ->
    gen_server:cast(Self, {leave_request, Ref}).
do_leave_request(Ref) ->
    supercast_proc:leave_ack(Ref).

info_request(_,_,_) -> ok.

%%----------------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%----------------------------------------------------------------------------
init([]) ->
    {ok, Read}  = application:get_env(monitor, master_chan_read_perm),
    {ok, Write} = application:get_env(monitor, master_chan_write_perm),
    Perm = #perm_conf{read=Read,write=Write},
    supercast_proc:new_channel(?MASTER_CHANNEL, ?MODULE, self(), Perm),
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
handle_cast({sync_request, CState, Ref}, S) ->
    SyncPdus0 = [],

    % get supercast sync dir generate temporary dir
    {ok, DumpDir} = application:get_env(monitor, http_sync_dir),
    TmpDir = monitor:generate_temp_dir(),
    DumpPath = filename:join(DumpDir,TmpDir),
    file:make_dir(DumpPath),

    % dump latest SQL events to dump path
    {ok, LatestEventsFile} = j_server_eventdb:dump_latest_events(DumpPath),

    % build and send pdu for client
    BeginPdu = monitor_pdu:masterSyncBegin(TmpDir,LatestEventsFile),
    SyncPdus1 = [BeginPdu|SyncPdus0],

    % filter and build target pdus
    {ok, TState} = monitor_data_master:iterate(target, fun(T,Acc) ->
        #target{permissions=Perm} = T,
        case supercast:satisfy(CState, Perm) of
            true    ->
                Pdu = monitor_pdu:infoTargetCreate(T),
                [Pdu|Acc];
            false   -> Acc
        end
    end),

    % send the target count to the client
    TStateCount = length(TState),
    TStateCountPdu = monitor_pdu:masterTargetCount(TStateCount),
    SyncPdus2 = [TStateCountPdu|SyncPdus1],

    SyncPdus3 = lists:append(TState,SyncPdus2),

    % filter and build probes pdus
    {ok, PState} = monitor_data_master:iterate(probe, fun(P,Acc) ->
        #probe{name=Name,permissions=Perm} = P,
        case supercast:satisfy(CState, Perm) of
            true    ->
                Pdu = monitor_pdu:infoProbeCreate(P),
                [{Name, Pdu} | Acc];
            false   -> ok
        end
    end),

    % send the probe count to the client
    PStateCount = length(PState),
    PStateCountPdu = monitor_pdu:masterProbeCount(PStateCount),
    SyncPdus4 = [PStateCountPdu|SyncPdus3],

    ProbesPdus = [P || {_,P} <- PState],
    SyncPdus5 = lists:append(ProbesPdus, SyncPdus4),

    SyncPdu6 =[monitor_pdu:masterSyncEnd() | SyncPdus5],

    supercast_proc:join_accept(Ref, lists:reverse(SyncPdu6)),

    %% trigger nchecks dummy reply
    lists:foreach(fun({PName,_}) ->
        monitor:trigger_nchecks_reply(PName, CState)
    end, PState),

    {noreply, S};

handle_cast({leave_request,Ref}, S) ->
    do_leave_request(Ref),
    {noreply, S};

handle_cast({send_unicast, CState, Pdu}, S) ->
    supercast_proc:send_unicast(?MASTER_CHANNEL, CState, [Pdu]),
    {noreply, S};

handle_cast(_R, S) ->
    {noreply, S}.

handle_info({mnesia_table_event,
        {write, target, Target, [], _ActivityId}}, S) ->
    handle_target_create(Target),
    {noreply, S};
handle_info({mnesia_table_event,
        {write, target, Target, [Target], _ActivityId}}, S) ->
    % same thing do nothing
    {noreply, S};
handle_info({mnesia_table_event,
        {write, target, NewTarget, OldTarget, _ActivityId}}, S) ->
    handle_target_update(NewTarget, OldTarget),
    {noreply, S};

handle_info({mnesia_table_event,
        {write, probe, Probe, [], _ActivityId}}, S) ->
    handle_probe_create(Probe),
    {noreply, S};
handle_info({mnesia_table_event,
        {write, probe, NewProbe, [OldProbe], _ActivityId}}, S) ->
    handle_probe_update(NewProbe, OldProbe),
    {noreply, S};

handle_info({mnesia_table_event,
        {write, job, Job, [], _ActivityId}}, S) ->
    handle_job_create(Job),
    {noreply, S};
handle_info({mnesia_table_event,
        {write, job, Job, [Job], _ActivityId}}, S) ->
    % same thing do nothing
    {noreply, S};
handle_info({mnesia_table_event,
        {write, job, NewJob, [OldJob], _ActivityId}}, S) ->
    handle_job_update(NewJob, OldJob),
    {noreply, S};


handle_info({mnesia_table_event,
        {write, dependency, Dep, [], _ActivityId}}, S) ->
    handle_dependency_create(Dep),
    {noreply, S};
handle_info({mnesia_table_event,
        {write, dependency, Dep, [_OldDep], _ActivityId}}, S) ->
    handle_dependency_update(Dep),
    {noreply, S};


handle_info({mnesia_table_event,
        {delete, _Table, What, [OldRecord], _ActivityId}}, S) ->
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
    supercast_proc:send_multicast(?MASTER_CHANNEL, [Pdu], Perm).

handle_target_update(#target{permissions=Perm} = Target, _) ->
    Pdu = monitor_pdu:infoTargetUpdate(Target),
    supercast_proc:send_multicast(?MASTER_CHANNEL, [Pdu], Perm).


handle_probe_create(#probe{permissions=Perm} = Probe) ->
    Pdu = monitor_pdu:infoProbeCreate(Probe),
    supercast_proc:send_multicast(?MASTER_CHANNEL, [Pdu], Perm).

handle_probe_update(#probe{permissions=Perm} = Probe,_) ->
    Pdu = monitor_pdu:infoProbeUpdate(Probe),
    supercast_proc:send_multicast(?MASTER_CHANNEL, [Pdu], Perm).


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
    supercast_proc:send_multicast(?MASTER_CHANNEL, [Pdu], Perm);
handle_delete({probe, _}, #probe{permissions=Perm} = Probe) ->
    Pdu = monitor_pdu:deleteProbe(Probe),
    supercast_proc:send_multicast(?MASTER_CHANNEL, [Pdu], Perm);
handle_delete({job, _Name},_) ->
    ?LOG_INFO("Delete job", _Name);
handle_delete({dependency, _Name},_) ->
    ?LOG_INFO("Delete dependency", _Name).

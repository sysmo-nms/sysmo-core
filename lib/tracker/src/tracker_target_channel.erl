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
% @doc
% launch/remove/add probes and switch messages. 
% <p>He is reponsible of
% <ul>
%   <li>switch probe messages (probe_evt) to the related modules depending 
%   on his type (activity, rrdmessages).</li>
%   <li>keep a clean state of target events for ifs when a client subscribe
%   to this channel</li>
%   <li> add, remove probes</li>
%   <li> keep his tracker_target_store record in sync</li>
% </ul>
% </p>
% @end
-module(tracker_target_channel).
-behaviour(gen_server).
-include("../include/tracker.hrl").

-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3]).

-export([
    start_link/2,
    launch_probes/1,
    new_event/3,
    update_rrd_file/2,
    subscribe/2,
    unsubscribe/1
]).

-record(chan_state, {
    chan_id,
    subscriber_count,
    target,
    probes_store,
    rrd_server
}).

%%-------------------------------------------------------------
%% API
%%-------------------------------------------------------------
% @private
-spec start_link(string(), #target{}) -> 
        {ok, pid()} | ignore | {error, any()}.
% @doc
% Start the server.
% @end
start_link(RrdDir, #target{id = Id} = Target) ->
    gen_server:start_link({local, Id}, ?MODULE, [RrdDir, Target], []).


% @private
-spec launch_probes(target_id()) -> ok | {error, any()}.
% @doc
% Once the server running, call him to initialize his probes.
% @end
launch_probes(Id) ->
    gen_server:call(Id, launch_probes).

% @private
-spec new_event(Self::pid(), ProbePid::pid(), Msg::tuple()) -> ok.
% @doc
% Called by one of the tracker_probes belonging to the tracker_target_channel
% identified by Self.
% @end
new_event(Self, ProbePid, Message) ->
    gen_server:cast(Self, {tracker_probe, ProbePid, Message}).

update_rrd_file(Server, RRD_update) ->
    gen_server:cast(Server, {rrd_update, RRD_update}).


-spec subscribe(target_id(), any()) -> ok.
% @doc
% ifs module related.
% The logic is here:
% - lock the server
% - dump every informations pending,
% - send all target related data to client,
% - unlock the server
% All this must be done in a single gen_server:call()
% @end
subscribe(TargetId, Client) ->
    gen_server:call(TargetId, {new_subscriber, Client}).

-spec unsubscribe(target_id()) -> ok.
% @doc
% ifs module related.
% TODO: Decrease the #chan_state.subscriber_count by 1. When the result = 0
% messages from this channel will no more be forwarded to ifs.
% @end
unsubscribe(TargetId) ->
    gen_server:call(TargetId, {one_subscriber_less}).

%%-------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%-------------------------------------------------------------
% @private
% @doc
% Initiate the gen server.
% @end
init([RootRrdDir, #target{
        id          = Id, 
        global_perm = Perm, 
        probes      = Probes} = Target]) ->
    % general logging
    gen_event:notify(tracker_events, {?MODULE, init, Id}),
    % ifs related
    ok = tracker_ifs_channel:chan_add({Id, Perm}),
    % rrd server
    RrdTargetDir = filename:join(RootRrdDir, atom_to_list(Id)),
    ok = rrd_dir_exist(RrdTargetDir),
    {ok, Pid} = errd_server_sup:create_instance(),
    errd_server:cd(Pid, RrdTargetDir),
    lists:foreach(fun(P) ->
        RCreate = P#probe.rrd_create,
        FName = filename:join(RrdTargetDir, RCreate#rrd_create.file),
        case filelib:is_file(FName) of
            true ->
                ok;
            false ->
                errd_server:command(Pid, P#probe.rrd_create)
        end
    end, Probes),
    
    % return
    {ok, 
        #chan_state{
            chan_id             = Id,
            subscriber_count    = 0,
            target              = Target,
            probes_store        = [],
            rrd_server          = Pid
        }
    }.


% @private
% @doc
% Initialize the probes.
% @end
handle_call(launch_probes, _F, #chan_state{target = Target} = S) ->
    Probes = Target#target.probes,
    ProbesStore = lists:foldl(fun(Probe, Accum) ->
        {ok, Pid}   = tracker_probe_sup:new({Target, Probe, self()}),
        tracker_probe:launch(Pid),
        [{Pid, Probe, error} | Accum]
    end, [], Probes),
    {reply, ok, S#chan_state{probes_store = ProbesStore}};

handle_call(_R, _F, S) ->
    {noreply, S}.




% @private
% CAST
% @doc
% Message sent by the modules.
% @end
handle_cast({tracker_probe, ProbePid, Msg}, 
        #chan_state{probes_store = ProbeStore} = S) ->
    {ProbePid, Probe, _Status} = lists:keyfind(ProbePid, 1, ProbeStore),
    probe_event(S, Probe, Msg),
    {noreply, S};

handle_cast({rrd_update, RRupdate}, #chan_state{rrd_server=RrdServer} = S) ->
    errd_server:command(RrdServer, RRupdate),
    {noreply, S};

handle_cast(_R, S) ->
    io:format("unknown cast ~p~n", [_R]),
    {noreply, S}.




% OTHER
% @private
handle_info(_I, S) ->
    {noreply, S}.

% @private
terminate(_R, #chan_state{target = Target} = _S) ->
    ok = tracker_ifs_channel:chan_del(Target#target.id),
    normal.

% @private
code_change(_O, S, _E) ->
    {ok, S}.





%% PRIVATE FUNCTIONS
% @private
-spec rrd_dir_exist(string()) -> ok | {error, any()}.
rrd_dir_exist(RrdDir) ->
    case file:read_file_info(RrdDir) of
        {ok, _} ->
            ok;
        {error, enoent} ->
            file:make_dir(RrdDir);
        Other ->
            Other
    end.

% @private
-spec probe_event(#probe{}, #chan_state{}, any()) -> ok.
probe_event(Chan, Probe, {'CRITICAL', _} = Msg) ->
    notify(status, Msg, Chan, Probe);

probe_event(Chan, Probe, {'WARNING', _}  = Msg) ->
    notify(status, Msg, Chan, Probe);

probe_event(Chan, #probe{type = status} = Probe, {'RECOVERY', _} = Msg) ->
    notify(status, Msg, Chan, Probe);

probe_event(Chan, #probe{type = fetch, rrd_update = RUpdate} = Probe, 
        {'RECOVERY', Val} = Msg) ->
    tracker_target_channel:update_rrd_file(Chan#chan_state.chan_id, RUpdate(Val)),
    notify(fetch, Msg, Chan, Probe);

probe_event(Chan, #probe{type = status} = Probe, {'OK', _} = Msg) ->
    notify(status, Msg, Chan, Probe);

probe_event(Chan, #probe{type = fetch, rrd_update = RUpdate} = Probe, 
        {'OK', Val} = Msg) ->
    tracker_target_channel:update_rrd_file(Chan#chan_state.chan_id, RUpdate(Val)),
    notify(fetch, Msg, Chan, Probe);

probe_event(Chan, Probe, {'UNKNOWN', _} = Msg) ->
    notify(fetch, Msg, Chan, Probe);

probe_event(_A,_B,C) ->
    io:format("Other ~p~n", [C]).


% @private
-spec notify(atom(), tuple(), #chan_state{}, #probe{}) -> ok.
% @doc
% Will log everything and also to ifs if #chan_state.subscribers_count > 0
% @end
notify(Type, {'OK',Val} = Msg, #chan_state{subscriber_count = 0} = Chan, Probe) ->
    ifs_mpd:handle_msg(
        {modTrackerPDU,
            {fromServer,
                {probeInfo, 
                    {'ProbeInfo',
                        atom_to_list(Chan#chan_state.chan_id),
                        Probe#probe.id,
                        Type,
                        Val }}}}),
    gen_event:notify(tracker_events, {tracker_probe, Type, Msg,
            {Chan#chan_state.chan_id, Probe#probe.id}});

notify(Type, {'RECOVERY',Val} = Msg, #chan_state{subscriber_count = 0} = Chan, Probe) ->
    ifs_mpd:handle_msg(
        {modTrackerPDU,
            {fromServer,
                {probeInfo, 
                    {'ProbeInfo',
                        atom_to_list(Chan#chan_state.chan_id),
                        Probe#probe.id,
                        Type,
                        Val }}}}),
    gen_event:notify(tracker_events, {tracker_probe, Type, Msg,
            {Chan#chan_state.chan_id, Probe#probe.id}});

notify(Type, Msg, Chan, Probe) ->
    % TODO notify ifs
    gen_event:notify(tracker_events, {tracker_probe, Type, Msg,
            {Chan#chan_state.chan_id, Probe#probe.id}}).

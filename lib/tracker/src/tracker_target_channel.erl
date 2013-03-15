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
%   <li> notify tracker_master_channel of general status wich allow non
%   subscribers of the channel to have basic informations (ex: status)</li>
% </ul>
% </p>
% <p>
%   A client subscribing to this channel must also be registered to 
%   'target-MasterChannel' to receive ALL events produced by the channel.
% </p>
% @end
-module(tracker_target_channel).
-behaviour(gen_server).
-behaviour(gen_channel).
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
    close_chan/1,
    dump/1
]).

-record(chan_state, {
    chan_id,
    subscriber_count,
    target,
    rrd_server,
    rrd_dir
}).

%%-------------------------------------------------------------
%% API
%%-------------------------------------------------------------
% @private
-spec start_link(string(), #target{}) -> 
        {ok, pid()} | ignore | {error, any()}.
% @doc
% Start the server. Args must be the rrd data root dir and a target record.
% @end
start_link(RrdDir, #target{id = Id} = Target) ->
    gen_server:start_link({local, Id}, ?MODULE, [RrdDir, Target], []).


% @private
-spec launch_probes(target_id()) -> ok | {error, any()}.
% @doc
% Once the server running, call him to initialize his probes. This is done
% by the "init_probes" phase of the traget_app module.
% @end
launch_probes(Id) ->
    gen_server:call(Id, launch_probes).

% @private
-spec new_event(Self::pid(), ProbePid::pid(), Msg::tuple()) -> ok.
% @doc
% Called by one of the tracker_probes belonging to the tracker_target_channel
% identified by Self.
% @end
new_event(Chan, ProbePid, Message) ->
    gen_server:cast(Chan, {tracker_probe_msg, ProbePid, Message}).

-spec update_rrd_file(pid(), tuple()) -> ok.
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
% All this can be done by using a single gen_server:call()
% @end
subscribe(TargetId, Client) ->
    gen_server:call(TargetId, {new_subscriber, Client}).

-spec close_chan(target_id()) -> ok.
close_chan(TargetId) ->
    gen_server:call(TargetId, terminate).

-spec dump(target_id()) -> #chan_state{}.
dump(Id) ->
    gen_server:call(Id, dump).
%%-------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%-------------------------------------------------------------
% @private
% @doc
% Initiate the gen server.
% @end
init([RootRrdDir, #target{
        id          = Id, 
        probes      = Probes} = Target]) ->
    % general logging
    gen_event:notify(tracker_events, {?MODULE, init, Id}),
    % ifs related
    ok = tracker_master_channel:chan_add(Target),
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
            chan_id             = Target#target.id, % for ese
            subscriber_count    = 0,
            target              = Target,
            rrd_server          = Pid,
            rrd_dir             = RrdTargetDir
        }
    }.


% @private
% @doc
% Initialize the probes.
% @end
handle_call(launch_probes, _F, #chan_state{target = Target} = S) ->
    Probes = Target#target.probes,
    NewProbes = lists:foldl(fun(Probe, Accum) ->
        {ok, Pid}   = tracker_probe_sup:new({Target, Probe, self()}),
        tracker_probe:launch(Pid),
        [ Probe#probe{pid = Pid} | Accum]
    end, [], Probes),
    NewTarget = Target#target{probes = NewProbes},
    NewState  = S#chan_state{target = NewTarget},
    {reply, ok, NewState};

% this call is used by ifs_mpd only, without API.
handle_call(get_perms, _F, #chan_state{target = Target} = S) ->
    {reply, Target#target.global_perm, S};

handle_call({synchronize, CState}, _F, 
        #chan_state{
            target = Target,
            chan_id = ChanId,
            rrd_dir = RrdDir} = S) ->
    % - send all datas to client then
    lists:foreach(fun(Probe) ->
        case Probe#probe.type of
            fetch ->
                %io:format("ici ~p~n", [pdu(probe_dump, Probe, ChanId)]),
                ifs_mpd:unicast_msg(CState, {
                    Probe#probe.permissions,
                    pdu(probe_dump, Probe, ChanId, RrdDir)
                });
            status ->
                ok
        end
    end, Target#target.probes),
    % - ifs_mpd:subscribe_stage3/2 and all will be ok
    io:format("ChanId is ~p dump is sent. Now subscribe_stage3~n", [ChanId]),
    ifs_mpd:subscribe_stage3(ChanId, CState),
    {reply, ok, S};

handle_call(dump, _F, S) ->
    {reply, S, S};

handle_call(terminate, _F, S) ->
    {stop, normal, ok, S};


handle_call(_R, _F, S) ->
    {noreply, S}.




% @private
% CAST
% @doc
% Message sent by the probes via new_event.
% @end
handle_cast({tracker_probe_msg, ProbePid, {status_move, {Status, _} = Msg}},
        #chan_state{target = Target} = S) ->
    Probes      = Target#target.probes,
    Probe       = lists:keyfind(ProbePid, 3, Probes),
    NewProbe    = Probe#probe{status = Status},
    NewProbes   = lists:keyreplace(ProbePid, 3, Probes, NewProbe),
    NewTarget   = Target#target{probes = NewProbes},
    NewState    = S#chan_state{target = NewTarget},

    tracker_master_channel:chan_event(probe_status_move,{NewTarget,NewProbe}),
    probe_event(NewState, NewProbe, Msg),
    {noreply, NewState};

handle_cast({tracker_probe_msg, ProbePid, {up, Status}},
        #chan_state{target = Target} = S) ->
    io:format("probe is up~n"),
    Probes      = Target#target.probes,
    Probe       = lists:keyfind(ProbePid, 3, Probes),
    NewProbe    = Probe#probe{status = Status},
    NewProbes   = lists:keyreplace(ProbePid, 3, Probes, NewProbe),
    NewTarget   = Target#target{probes = NewProbes},
    NewState    = S#chan_state{target = NewTarget},

    tracker_master_channel:chan_event(probe_status_move,{NewTarget,NewProbe}),
    {noreply, NewState};

handle_cast({tracker_probe_msg, ProbePid, {down, _Reason}},
        #chan_state{target = Target} = S) ->
    io:format("probe is down~n"),
    Probes      = Target#target.probes,
    Probe       = lists:keyfind(ProbePid, 3, Probes),
    NewProbe    = Probe#probe{status = 'INITIAL'},
    NewProbes   = lists:keyreplace(ProbePid, 3, Probes, NewProbe),
    NewTarget   = Target#target{probes = NewProbes},
    NewState    = S#chan_state{target = NewTarget},

    tracker_master_channel:chan_event(probe_status_move,{NewTarget,NewProbe}),
    {noreply, NewState};

handle_cast({tracker_probe_msg, ProbePid, Msg}, 
        #chan_state{target = Target} = S) ->
    Probes      = Target#target.probes,
    Probe       = lists:keyfind(ProbePid, 3, Probes),
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
terminate(R, #chan_state{target = Target} = _S) ->
    ok = tracker_master_channel:chan_del(Target),
    R.

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
    tracker_target_channel:update_rrd_file(
            Chan#chan_state.chan_id, RUpdate(Val)),
    notify(fetch, Msg, Chan, Probe);

probe_event(Chan, #probe{type = status} = Probe, {'OK', _} = Msg) ->
    notify(status, Msg, Chan, Probe);

probe_event(Chan, #probe{type = fetch, rrd_update = RUpdate} = Probe, 
        {'OK', Val} = Msg) ->
    tracker_target_channel:update_rrd_file(
            Chan#chan_state.chan_id, RUpdate(Val)),
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
notify(Type, {'OK',Val} = Msg, 
        #chan_state{subscriber_count = 0} = Chan, 
        #probe{permissions = Perm} = Probe) ->
    ifs_mpd:multicast_msg(Chan#chan_state.chan_id, {Perm,
        {modTrackerPDU,
            {fromServer,
                {probeFetch, 
                    {'ProbeFetch',
                        atom_to_list(Chan#chan_state.chan_id),
                        Probe#probe.id,
                        Type,
                        Val }}}}}),
    gen_event:notify(tracker_events, {tracker_probe, Type, Msg,
            {Chan#chan_state.chan_id, Probe#probe.id}});

notify(Type, {'RECOVERY',Val} = Msg, 
        #chan_state{subscriber_count = 0}   = Chan, 
        #probe{permissions = Perm}          = Probe) ->
    ifs_mpd:multicast_msg(Chan#chan_state.chan_id,{Perm, 
            {modTrackerPDU,
                {fromServer,
                    {probeFetch, 
                        {'ProbeFetch',
                            atom_to_list(Chan#chan_state.chan_id),
                            Probe#probe.id,
                            Type,
                            Val }}}}}),
    gen_event:notify(tracker_events, {tracker_probe, Type, Msg,
            {Chan#chan_state.chan_id, Probe#probe.id}});

notify(Type, Msg, Chan, Probe) ->
    % TODO notify ifs
    gen_event:notify(tracker_events, {tracker_probe, Type, Msg,
            {Chan#chan_state.chan_id, Probe#probe.id}}).

pdu(probe_dump, Probe, ChanId, RrdDir) ->
    {modTrackerPDU,
        {fromServer,
            {probeDump,
                {'ProbeDump',
                    atom_to_list(ChanId),
                    Probe#probe.id,
                    Probe#probe.type,
                    gen_rrdfile(Probe,RrdDir)
                }
            }
        }
    }.

gen_rrdfile(Probe, RrdDir) ->
    ProbeName   = Probe#probe.name,
    [ProbeId]   = io_lib:format("~p", [Probe#probe.id]),
    One         = string:concat(ProbeName, "-"),
    Two         = string:concat(One, ProbeId),
    Three       = string:concat(Two, ".rrd"),
    {ok, BinFile} = file:read_file(filename:join(RrdDir, Three)),
    BinFile.

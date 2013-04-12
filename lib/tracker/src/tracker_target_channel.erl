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
%   <li>keep a clean state of target events for supercast when a client subscribe
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

% GEN_SERVER CALLBACKS
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3]).

% API
-export([
    start_link/2,
    cold_start/1,
    update/3,
    subscribe/2,
    dump/1
]).

-record(state, {
    chan_id,
    subscriber_count,
    target
}).


%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
start_link(DataDir, #target{id = Id} = Target) ->
    gen_server:start_link({local, Id}, ?MODULE, [DataDir, Target], []).

-spec cold_start(pid()) -> ok | {error, any()}.
% @doc
% Once the server running, call him to start his probes. This is done
% by the "cold_start" phase of the traget_app module.
% @end
cold_start(Pid) ->
    gen_server:call(Pid, cold_start).

-spec update(Self::pid(), integer(), Msg::tuple()) -> ok.
% @doc
% Called by one of the tracker_probes belonging to the tracker_target_channel
% identified by Chan. Needed for handling of client synchronisation.
% A tracker_probe will call this function at probe status change, or when
% there is a need to update a probe file.
% @end
update(Chan, ProbeId, Message) ->
    gen_server:cast(Chan, {update, ProbeId, Message}).

-spec subscribe(target_id(), any()) -> ok.
% @doc
% supercast module related.
% The logic is here:
% - dump every informations pending,
% - send all target related data to client,
% All this must be done by using a single gen_server:call() fully
% synchronize the client.
% @end
subscribe(TargetId, Client) ->
    gen_server:call(TargetId, {new_subscriber, Client}).

% @doc
% DEBUG function
% @end
dump(Id) ->
    gen_server:call(Id, dump).



%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% GEN_SERVER CALLBACKS 
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% INIT       
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
init([RootDataDir, Target]) ->
    ok              = init_snmp(Target),
    {ok, Target1}   = init_dir(Target, RootDataDir),
    {ok, TargetF}   = init_probes(Target1),

    {ok, 
        #state{
            chan_id             = Target#target.id,     % shortcut
            subscriber_count    = 0,   
            target              = TargetF
        }
    }.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% HANDLE_CALL
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------


%%----------------------------------------------------------------------------
%% SELF API CALLS 
%%----------------------------------------------------------------------------
handle_call(cold_start, _F, #state{target = Target} = S) ->
    lists:foreach(fun(#probe{pid = Pid}) ->
        tracker_probe:cold_start(Pid)
    end, Target#target.probes),
    {reply, ok, S};


%%----------------------------------------------------------------------------
%% CALLS VIA GEN_CHANNEL BEHAVIOUR
%%----------------------------------------------------------------------------
handle_call(get_perms, _F, #state{target = Target} = S) ->
    {reply, Target#target.global_perm, S};

handle_call({synchronize, _IfsCState}, _F, S) ->
    {reply, ok, S};

handle_call(dump, _F, S) ->
    {reply, S, S};

handle_call(_R, _F, S) ->
    {noreply, S}.


%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% HANDLE_CAST
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
% channel event, will not be forwarded to 'tartet-MasterChannel'
handle_cast({update, ProbeId, {channel_event, Msg}}, 
        #state{target = #target{probes = Probes}} = S) ->
    #probe{pid = Pid} = lists:keyfind(ProbeId, 2, Probes),
    tracker_probe:loggers_update(Pid, Msg),
    % TODO 
    % SUPERCAST send
    {noreply, S};

% master_event, will not be forwarded to the subscribers of this
% channel, but to 'target-MasterChannel'.
handle_cast({update, ProbeId, {master_event, {probe_status_move, NewStatus}}},
        #state{target = #target{probes = Probes} = Target} = S) ->
    Probe   = lists:keyfind(ProbeId, 2, Probes),
    NProbes = lists:keyreplace(
            ProbeId, 2, Probes, Probe#probe{status = NewStatus}),
    % TODO
    % MASTERCHAN send
    io:format("~p PROBE STATUS MOVE ~p~n", [?MODULE, Probe#probe.id]),
    S2      = S#state{target = Target#target{probes = NProbes}},
    {noreply, S2};

% master_event, will not be forwarded to the subscribers of this
% channel, but to 'target-MasterChannel'.
handle_cast({update, _ProbeId, {master_event, {property_set, _Properties}}},
        #state{target = _Target} = S) ->
    % TODO
    % MASTERCHAN send
    io:format("property set ~p~n", [_Properties]),
    {noreply, S};

handle_cast(_R, S) ->
    io:format("unknown cast ~p~n", [_R]),
    {noreply, S}.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% HANDLE_INFO
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
handle_info(_I, S) ->
    {noreply, S}.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% TERMINATE  
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
terminate(R, #state{target = Target} = _S) ->
    ok = tracker_master_channel:chan_del(Target),
    R.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% CODE_CHANGE
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
code_change(_O, S, _E) ->
    {ok, S}.





%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% IFS UTILS
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
% -spec notify(atom(), tuple(), #state{}, #probe{}) -> ok.
% % @doc
% % Will log everything and also to supercast if #state.subscribers_count > 0
% % @end
% notify(Type, {'OK',Val} = Msg, 
%         #state{subscriber_count = 0} = Chan, 
%         #probe{permissions = Perm} = Probe) ->
%     supercast_mpd:multicast_msg(Chan#state.chan_id, {Perm,
%         {modTrackerPDU,
%             {fromServer,
%                 {probeFetch, 
%                     {'ProbeFetch',
%                         atom_to_list(Chan#state.chan_id),
%                         Probe#probe.id,
%                         Type,
%                         Val }}}}}),
%     gen_event:notify(tracker_events, {tracker_probe, Type, Msg,
%             {Chan#state.chan_id, Probe#probe.id}});
% 
% notify(Type, {'RECOVERY',Val} = Msg, 
%         #state{subscriber_count = 0}   = Chan, 
%         #probe{permissions = Perm}          = Probe) ->
%     supercast_mpd:multicast_msg(Chan#state.chan_id,{Perm, 
%             {modTrackerPDU,
%                 {fromServer,
%                     {probeFetch, 
%                         {'ProbeFetch',
%                             atom_to_list(Chan#state.chan_id),
%                             Probe#probe.id,
%                             Type,
%                             Val }}}}}),
%     gen_event:notify(tracker_events, {tracker_probe, Type, Msg,
%             {Chan#state.chan_id, Probe#probe.id}});
% 
% notify(Type, Msg, Chan, Probe) ->
%     % notify supercast
%     gen_event:notify(tracker_events, {tracker_probe, Type, Msg,
%             {Chan#state.chan_id, Probe#probe.id}}).
% 
% pdu(probe_dump, Probe, ChanId, DataDir) ->
%     {modTrackerPDU,
%         {fromServer,
%             {probeDump,
%                 {'ProbeDump',
%                     atom_to_list(ChanId),
%                     Probe#probe.id,
%                     Probe#probe.type
%                     %gen_rrdfile(Probe,DataDir)
%                 }
%             }
%         }
%     }.


%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% PRIVATE
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
% @private
get_opt(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        {_, Conf} ->
            Conf;
        _ ->
            false
    end.

% @private
init_dir(#target{id = Id} = Target, RootDataDir) ->
    TargetDir = filename:join(RootDataDir, atom_to_list(Id)),
    case file:read_file_info(TargetDir) of
        {ok, _} ->
            {ok, Target#target{directory = TargetDir}};
        {error, enoent} ->
            ok = file:make_dir(TargetDir),
            {ok, Target#target{directory = TargetDir}};
        {error, Other} ->
            {error, Other}
    end.

% @private
init_probes(#target{probes = Probes} = Target) ->
    ProbesF = lists:foldl(fun(Probe, Accum) ->
        {ok, Pid} = tracker_probe_sup:new({Target, Probe}),
        [Probe#probe{pid = Pid} | Accum]
    end, [], Probes),
    {ok, Target#target{probes = ProbesF}}.

% @private
init_snmp(#target{id = Id, properties = Properties}) ->
    esnmp_user_v2:agent(Id, get_opt(snmp_conf, Properties)).

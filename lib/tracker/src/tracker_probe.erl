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
% @end
-module(tracker_probe).
-behaviour(gen_server).
-include("../include/tracker.hrl").

-export([
    init/1,
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

-export([
    start_link/1,
    launch/1,
    notify/2,
    probe_pass/3 % do not use. Only exported for timer:apply_after()
]).

%%-------------------------------------------------------------
%% API
%%-------------------------------------------------------------
% @private
-spec start_link({#target{}, #probe{}, pid()}) -> {ok, pid()}.
% @doc 
% Start the probe server. Called by tracker_probe_sup:new/1.
% #target{} and #probe{} records will be sent as arguments to the "gen_probe"
% mobule of this server.
% @end
start_link({Target, Probe, Pid}) ->
    gen_server:start_link(?MODULE, [Target, Probe, Pid], []).

% @private
-spec launch(pid()) -> ok.
% @doc
% Once the server is started, it need to be initialized so he can then enter
% in an infinite loop. This function is called from his parent 
% tracker_target_channel() module.
% @end
launch(Pid) ->
    gen_server:call(Pid, start).

-spec notify(ProbePid::pid(), Message::tuple()) -> ok.
% @doc
% Used to send a notification to the tracker_target_channel who own the
% tracker_probe server ProbePid. It is exported because a gen_inspector can
% use it to send alert message like if it is the probe who send it.
% <p>
% Message must be of type {Key::atom(), Val::any()} and have these 
% significations:
% <ul>
%   <li>
%       Key = 'OK' | 'RECOVERY'. If probe is of type 'fetch', then Val is
%       interpreted as a value to enter on the rrd file associated.
%   </li>
%   <li>
%       Key = 'CRITICAL'. Will trigger a warning fun associated with the
%       probe.
%   </li>
%   <li>
%       Key = Other, will log the message on tracker_events::module().
%   </li>
% </ul>
% </p>
% @end
notify(ProbePid, Message) ->
    gen_server:cast(ProbePid, {notify, Message}).

%%-------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%-------------------------------------------------------------
% @private
init([Target, Probe, Pid]) ->
    State = #probe_server_state{
        target_chan     = Pid,
        target          = Target,
        probe           = Probe, 
        step            = Probe#probe.step,
        timeout_max     = Probe#probe.timeout_max,
        timeout_wait    = Probe#probe.timeout_wait,
        inspectors      = Probe#probe.inspectors,
        timeout_current = 0,
        status          = error
    },
    {ok, State}.
        
    
% launch a probe for the first time. Give a way to inspectors to initialize
% themself and Randomise start.
% @private
handle_call(start, _F, #probe_server_state{
        probe       = Probe, 
        target      = Target, 
        step        = Step} = S) ->
    InitialLaunch = tracker_misc:random(Step * 1000),
    timer:apply_after(InitialLaunch, ?MODULE, 
            probe_pass, [Target, Probe, self()]),
    notify(self(), {'INFO', start}),
    {reply, ok, init_inspect(S)};

handle_call(_R, _F, S) ->
    {noreply, S}.


%%-------------------------------------------------------------
% HANDLE_CAST
%%-------------------------------------------------------------
% launch a probe normal
% @private
handle_cast(launch, #probe_server_state{
        probe   = Probe, 
        target  = Target, 
        step    = Frequency} = S) ->
    timer:apply_after(Frequency * 1000, 
            ?MODULE, probe_pass, [Target, Probe, self()]),
    {noreply, S};

handle_cast({notify, Message}, #probe_server_state{
        target_chan = ChanPid} = S) ->
    tracker_target_channel:new_event(ChanPid, self(), Message),
    {noreply, S};

% error, max timeout reached, and status is ok
handle_cast({probe_response, {error, Val} = Msg}, #probe_server_state{
        timeout_current = T, 
        timeout_max     = T, 
        status          = ok} = S) ->
    %% ici une alerte est declanchee
    notify(self(), {'CRITICAL', Val}),
    gen_server:cast(self(), launch),
    {noreply, inspect(S#probe_server_state{status = error}, Msg) };

% error, max timeout reached and status is allready error, renew a WARNING.
handle_cast({probe_response, {error, Val} = Msg}, #probe_server_state{
            timeout_current = T, 
            timeout_max     = T, 
            status          = error} = S) ->
    notify(self(), {'CRITICAL', Val}),
    gen_server:cast(self(), launch),
    {noreply, inspect(S, Msg)};

% error, but  max number of timeout not reached. It is a warning.
handle_cast({probe_response, {error, Val} = Msg}, #probe_server_state{
            timeout_current = T} = S) ->
    notify(self(), {'WARNING', Val}),
    gen_server:cast(self(), launch),
    {noreply, inspect(S#probe_server_state{timeout_current = T + 1}, Msg)};

% ok but status was error, then it is a recovery. 
handle_cast({probe_response, {ok, Val} = Msg}, #probe_server_state{
        status      = error} = S) ->
    notify(self(), {'RECOVERY', Val}),
    gen_server:cast(self(), launch),
    {noreply, 
        inspect(S#probe_server_state{timeout_current = 0, status = ok}, Msg)};

% ok standard, reset the timeout_current count. It is an informational msg.
handle_cast({probe_response, {ok, Val} = Msg}, S) ->
    notify(self(), {'OK', Val}),
    gen_server:cast(self(), launch),
    {noreply, inspect(S#probe_server_state{timeout_current = 0}, Msg)};

handle_cast(_R, S) ->
    io:format("Unknown message ~p ~p ~p ~p~n", [?MODULE, ?LINE, _R, S]),
    {noreply, S}.


% OTHER
% @private
handle_info(I, S) ->
    io:format("handle_info ~p ~p ~p~n", [?MODULE, I, S]),
    {noreply, S}.

% @private
terminate(_R, _S) ->
    %notify(self(), {'INFO', terminate}),
    normal.

% @private
code_change(_O, S, _E) ->
    io:format("code_change ~p ~p ~p ~p~n", [?MODULE, _O, _E, S]),
    {ok, S}.



%% PRIVATE FUNCT

% @private
-spec probe_pass(#target{}, #probe{}, pid()) -> ok.
% @doc
% It is the spawned proc who call the "gen_probe" module defined in the 
% #probe{} record.
% @end
probe_pass(Target, Probe, Pid) ->
    Fun     = Probe#probe.func,
    Result  = Fun({Target, Probe}),
    gen_server:cast(Pid, {probe_response, Result}).


% @private
-spec init_inspect(#probe_server_state{}) -> #probe_server_state{}.
init_inspect(#probe_server_state{inspectors = Inspectors} = State) ->
    NewState = lists:foldl(fun({Mod, Args}, S) ->
        {ok, NewS} = Mod:init(Args, S),
        NewS
    end, State, Inspectors),
    NewState.

% @private
-spec inspect(#probe_server_state{}, Msg::tuple()) -> #probe_server_state{}.
inspect(#probe_server_state{inspectors = Inspectors} = State, Msg) ->
    {State, NewState, Msg} = lists:foldl(
        fun({Mod, _}, {Orig, Modified, Message}) ->
            {ok, New} = Mod:inspect(Orig, Modified, Message),
            {Orig, New, Message}
        end, {State, State, Msg}, Inspectors),
    NewState.

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
    probe_pass/3
]).

%%-------------------------------------------------------------
%% API
%%-------------------------------------------------------------
-spec start_link({#target{}, #probe{}, pid()}) -> {ok, pid()}.
% @doc 
% Start the probe server.
% #target{} and #probe{} records will be sent as arguments to the "gen_probe"
% mobule of this server.
% @end
start_link({Target, Probe, Pid}) ->
    gen_server:start_link(?MODULE, [Target, Probe, Pid], []).

-spec launch(pid()) -> ok.
% @doc
% Once the server is started, it need to be initialized so he can then enter
% in an infinite loop.
% @end
launch(Pid) ->
    gen_server:call(Pid, init_launch).

%%-------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%-------------------------------------------------------------
init([Target, Probe, Pid]) ->
    State = flipflap(init, #probe_server_state{
        target_chan     = Pid,
        target          = Target,
        probe           = Probe, 
        step            = Probe#probe.step,
        timeout_max     = Probe#probe.timeout_max,
        timeout_wait    = Probe#probe.timeout_wait,
        timeout_current = 0,
        status          = error
    }),
    {ok, State}.
        
    
% launch a probe for the first time. Randomise start
handle_call(init_launch, _F, #probe_server_state{
        probe       = Probe, 
        target      = Target, 
        step        = Step, 
        target_chan = ChanPid} = S) ->
    InitialLaunch = tracker_misc:random(Step * 1000),
    timer:apply_after(InitialLaunch, ?MODULE, 
            probe_pass, [Target, Probe, self()]),
    notify_chan(ChanPid, probe_init_ok),
    {reply, ok, S};

handle_call(_R, _F, S) ->
    {noreply, S}.


%%-------------------------------------------------------------
% HANDLE_CAST
%%-------------------------------------------------------------
% launch a probe normal
handle_cast(launch, #probe_server_state{
        probe   = Probe, 
        target  = Target, 
        step    = Frequency} = S) ->
    timer:apply_after(Frequency * 1000, 
            ?MODULE, probe_pass, [Target, Probe, self()]),
    {noreply, S};

% error, max timeout reached, and status is ok
handle_cast({probe_response, {error, Val}}, #probe_server_state{
        timeout_current = T, 
        timeout_max     = T, 
        target_chan     = ChanPid, 
        status          = ok} = S) ->
    %% ici une alerte est declanchee
    notify_chan(ChanPid, {'CRITICAL', Val}),
    gen_server:cast(self(), launch),
    {noreply, flipflap(inspect, S#probe_server_state{status = error})};

% error, max timeout reached but status is allready error, do nothing
handle_cast({probe_response, {error, _}}, #probe_server_state{
            timeout_current = T, 
            timeout_max     = T, 
            status          = error} = S) ->
    gen_server:cast(self(), launch),
    {noreply, flipflap(inspect, S)};

% error, but  max number of timeout not reached. It is a warning.
handle_cast({probe_response, {error, Val}}, #probe_server_state{
            timeout_current = T, 
            target_chan     = ChanPid} = S) ->
    notify_chan(ChanPid, {'WARNING', Val}),
    gen_server:cast(self(), launch),
    {noreply, flipflap(inspect, 
        S#probe_server_state{timeout_current = T + 1}
    )};

% ok but status was error, then it is a recovery
handle_cast({probe_response, {ok, Val}}, #probe_server_state{
        status      = error, 
        target_chan = ChanPid} = S) ->
    notify_chan(ChanPid, {'RECOVERY', Val}),
    gen_server:cast(self(), launch),
    {noreply, flipflap(inspect, 
        S#probe_server_state{timeout_current = 0, status = ok}
    )};

% ok standard, reset the timeout_current count. It is an informational msg
handle_cast({probe_response, {ok, Val}}, #probe_server_state{
        target_chan = ChanPid} = S) ->
    notify_chan(ChanPid, {'OK', Val}),
    gen_server:cast(self(), launch),
    {noreply, flipflap(inspect, S#probe_server_state{timeout_current = 0})};

handle_cast(_R, S) ->
    {noreply, S}.


% OTHER
handle_info(I, S) ->
    io:format("handle_info ~p ~p ~p~n", [?MODULE, I, S]),
    {noreply, S}.

terminate(_R, _S) ->
    io:format("terminate ~p ~p ~p~n", [?MODULE, _R, _S]),
    normal.

code_change(_O, S, _E) ->
    io:format("code_change ~p ~p ~p ~p~n", [?MODULE, _O, _E, S]),
    {ok, S}.


-spec probe_pass(#target{}, #probe{}, pid()) -> ok.
% @doc
% It is the spawned proc who call the "gen_probe" module defined in the 
% #probe{} record.
% @end
probe_pass(Target, Probe, Pid) ->
    Fun     = Probe#probe.func,
    Result  = Fun({Target, Probe}),
    gen_server:cast(Pid, {probe_response, Result}).

-spec notify_chan(pid(), any()) -> ok.
% @doc
% Notification to the tracker_target_channel module who initiate this server.
% @end
notify_chan(ChanPid, Msg) ->
    gen_server:cast(ChanPid, {probe_evt, self(), Msg}).

-spec flipflap(init | inspect, #probe_server_state{}) 
        -> #probe_server_state{}.
% @doc
% Called at each probe_pass to detect flipflap. 
% @end
flipflap(init, #probe_server_state{probe = Probe} = State) ->
    {ok, NewState} = (Probe#probe.flipflap_mod):init(State),
    NewState;

flipflap(inspect, #probe_server_state{probe = Probe} = State) ->
    {ok, NewState} = (Probe#probe.flipflap_mod):inspect(State),
    NewState.

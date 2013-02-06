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
-include_lib("../include/tracker.hrl").

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

-record(state, {
    target,
    probe,
    frequency,
    timeout_current,
    timeout_max,  % max series of timeout ocuring before trigger an alert
    timeout_wait, % wait for a responce in timeout_wait
    status        % ok | error.
}).

%%-------------------------------------------------------------
%% API
%%-------------------------------------------------------------
% @doc start the server.
start_link({Target, Probe}) ->
    gen_server:start_link(?MODULE, [Target, Probe], []).

launch(Pid) ->
    io:format("lllllllllllllllaunch~n~n"),
    gen_server:cast(Pid, init_launch),
    ok.

%%-------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%-------------------------------------------------------------
init([Target, Probe]) ->
    io:format("start probe ~p~p~n", [Target, Probe]),
    {ok, 
        #state{
            target          = Target,
            probe           = Probe, 
            frequency       = Probe#probe.frequency,
            timeout_max     = Probe#probe.timeout_max,
            timeout_wait    = Probe#probe.timeout_wait,
            timeout_current = 0,
            status          = error
        }
    }.
    
handle_call(_R, _F, S) ->
    {noreply, S}.


%%-------------------------------------------------------------
% HANDLE_CAST
%%-------------------------------------------------------------
% launch a probe for the first time. Randomise start
handle_cast(init_launch, #state{probe = Probe, target = Target, 
            frequency = Frequency} = S) ->
    InitialLaunch = tracker_misc:random(Frequency * 1000),
    timer:apply_after(InitialLaunch, 
            ?MODULE, probe_pass, [Target, Probe, self()]),
    {noreply, S};

% launch a probe normal
handle_cast(launch, #state{probe = Probe, target = Target, 
            frequency = Frequency} = S) ->
    timer:apply_after(Frequency * 1000, 
            ?MODULE, probe_pass, [Target, Probe, self()]),
    {noreply, S};

% error, max timeout reached, and status is ok
handle_cast({probe_response, {error, Val}}, 
        #state{timeout_current = T, timeout_max = T, status = ok} = S) ->
    %% ici une alerte est declanchee
    io:format("ALERT: reached max timeout ~p ~p~n", [Val, self()]),
    gen_server:cast(self(), launch),
    {noreply, S#state{status = error}};

% error, max timeout reached but status is allready error, do nothing
handle_cast({probe_response, {error, _}}, 
        #state{timeout_current = T, timeout_max = T, status = error} = S) ->
    gen_server:cast(self(), launch),
    {noreply, S};

% error, max timeout not reached
handle_cast({probe_response, {error, Val}}, 
        #state{timeout_current = T} = S) ->
    io:format("WARNING error ~p ~p~n", [Val, self()]),
    gen_server:cast(self(), launch),
    {noreply, S#state{timeout_current = T + 1}};

% ok but status was error, then it is a recovery
handle_cast({probe_response, {ok, Val}}, #state{status = error} = S) ->
    io:format("Recovery ~p ~p~n", [Val, self()]),
    gen_server:cast(self(), launch),
    {noreply, S#state{timeout_current = 0, status = ok}};

% ok standard, reset the timeout_current count
handle_cast({probe_response, {ok, Val}}, S) ->
    io:format("OK ~p ~p~n", [Val, self()]),
    gen_server:cast(self(), launch),
    {noreply, S#state{timeout_current = 0}};

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


% @private
probe_pass(Target, Probe, Pid) ->
    Fun     = Probe#probe.func,
    Result  = Fun({Target, Probe}),
    gen_server:cast(Pid, {probe_response, Result}).

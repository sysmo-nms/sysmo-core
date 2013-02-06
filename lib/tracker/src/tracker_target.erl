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
-module(tracker_target).
-behaviour(gen_server).
-include_lib("../include/tracker.hrl").

-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3]).

-export([
    start_link/2,
    launch_probes/1
]).

-record(chan_state, {
    chan_id,
    target,
    rrd_dir,
    probes_store
}).


%%-------------------------------------------------------------
%% API
%%-------------------------------------------------------------
% @doc start the server. No arguments.
start_link(RrdDir, #target{id = Id} = Target) ->
    gen_server:start_link({local, Id}, ?MODULE, [RrdDir, Target], []).

-spec launch_probes(target_id()) -> ok | {error, any()}.
% @doc
% Once the server running, call him to initialize his probes.
% @end
launch_probes(Id) ->
    %io:format("launch probe ~pn", [Id]).
    gen_server:call(Id, launch_probes).

%%-------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%-------------------------------------------------------------
init([RrdDir, #target{id = Id} = Target]) ->
    gen_event:notify(tracker_events, {chan_init, Id}),
    {ok, 
        #chan_state{
            chan_id = Id,
            target = Target,
            rrd_dir = RrdDir,
            probes_store = []
        }
    }.

handle_call(launch_probes, _F, #chan_state{target = Target} = S) ->
    Probes = Target#target.probes,
    ProbesStore = lists:foldl(fun(X, Accum) ->
        {ok, Pid} = tracker_probe_sup:new({Target, X}),
        tracker_probe:launch(Pid),
        ProbeId = X#probe.id,
        [{ProbeId, Pid} | Accum]
    end, [], Probes),
    {reply, ok, S#chan_state{probes_store = ProbesStore}};

handle_call(_R, _F, S) ->
    {noreply, S}.
% CAST
handle_cast(_R, S) ->
    {noreply, S}.

% OTHER
handle_info(_I, S) ->
    {noreply, S}.

terminate(_R, _S) ->
    normal.

code_change(_O, S, _E) ->
    {ok, S}.

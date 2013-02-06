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
-module(tracker_app).
-behaviour(application).

-export([
    start/2,
    start_phase/3,
    stop/1]).

start(_Type, _Args) ->
    {ok, GenEventListeners} = application:get_env(tracker, registered_events),
    {ok, DbDir}             = application:get_env(tracker, db_dir),
    {ok, RrdDir}            = application:get_env(tracker, rrd_dir),
    tracker_sup:start_link(
        GenEventListeners,
        filename:absname(DbDir), 
        filename:absname(RrdDir)
    ).

start_phase(init_chans, normal, []) ->
    AllTargets = tracker_target_store:info(),
    lists:foreach(fun(X) ->
        tracker_target_sup:new(X)
    end, AllTargets);

start_phase(launch_probes, normal, []) ->
    tracker_target_sup:init_launch_probes().

stop(_State) ->
	ok.

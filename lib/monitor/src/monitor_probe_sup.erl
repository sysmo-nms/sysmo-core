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
-module(monitor_probe_sup).
-behaviour(supervisor).

-export([
    start_link/0,
    new/1,
    launch/0
]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

new({Target, Probe}) ->
    supervisor:start_child(?MODULE, [{Target, Probe}]).

launch() ->
    Childs  = supervisor:which_children(?MODULE),
    PIds    = [Pid || {_,Pid,_,_} <- Childs],
    initialize_probes(PIds).

initialize_probes(Pids) ->
    freeze_probes(  Pids),
    reset_family(   Pids),
    sync_parents(   Pids),
    sync_childs(    Pids),
    launch_probes(  Pids).

freeze_probes([])       -> ok;
freeze_probes([H|T])    ->
    monitor_probe_fsm:freeze(H),
    freeze_probes(T).

reset_family([]) -> ok;
reset_family([H|T]) ->
    monitor_probe_fsm:reset_family(H),
    reset_family(T).

sync_parents([])     -> ok; 
sync_parents([H|T])  ->
    monitor_probe_fsm:synchronize_parents(H),
    sync_parents(T).

sync_childs([])     -> ok;
sync_childs([H|T]) ->
    monitor_probe_fsm:synchronize_childs(H),
    sync_childs(T).

launch_probes([])       -> ok;
launch_probes([H|T])    ->
    monitor_probe_fsm:launch(H),
    launch_probes(T).

init([]) ->
    {ok, 
        {
            {simple_one_for_one, 1, 60},
            [
                {
                    monitor_probe_fsm,
                    {monitor_probe_fsm, start_link, []},
                    transient,
                    2000,
                    worker,
                    [monitor_probe_fsm]
               }
            ]
        }
    }.


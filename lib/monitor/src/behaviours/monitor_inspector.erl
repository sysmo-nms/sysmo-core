% This file is part of "Enms" (http://sourceforge.net/projects/enms/)
% Copyright (C) 2012 <Sébastien Serre sserre.bx@gmail.com>
% 
% Enms is a Network Management System aimed to manage and monitor SNMP
% targets, monitor network hosts and services, provide a consistent
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
-module(monitor_inspector).
-include("include/monitor.hrl").

-export([
    init_all/1,
    inspect_all/3
]).

-callback info() -> {ok, Info::string()}.
% @doc
% Must return an Info string, wich explain what does the inspector.
% @end
-callback init(Conf::[any()], Probe::#probe{}) -> 
    {ok, State::any()}.
% @doc
% Called at the init stage. Must return a State value wich will be 
% given as argument to the inspect/4 callback.
% @end

-callback inspect(
        State           ::any(),
        ProbeReturn     ::#probe_return{},
        OrigProbe       ::#probe{},
        ModifiedProbe   ::#probe{}
    ) -> {ok, State::any(), NewProbe::#probe{}}.
% @doc
% Called each time a probe_return from the probe is received.
% State is the value returned by init/3. Probe is the original probe record
% before any inspectors, ModifiedProbe may have been modified by a 
% preceding inspector.
% Must return a possibly modified State and a possibly modified #probe{}.
% @end


-spec init_all(Probe::#probe{}) -> {ok, InspectorStates::[term()]}.
% @doc
% Used by the monitor_probe module to initialize all inspectors of a probe
% and return a list of inspectors states.
% @end
init_all(Probe) ->
    Inspectors  = Probe#probe.inspectors,
    States      = [],
    init_inspectors(Probe, Inspectors, States).

init_inspectors(_, [], InspectStates) ->
    {ok, InspectStates};
init_inspectors(Probe, [Inspector|Inspectors], InspectStates) ->
    Mod                 = Inspector#inspector.module,
    Conf                = Inspector#inspector.conf,
    {ok, InspReply}     = Mod:init(Conf, Probe),
    NewInspectStates    = lists:keystore(Mod,1,InspectStates,{Mod,InspReply}),
    init_inspectors(Probe, Inspectors, NewInspectStates).

-spec inspect_all(States::[term()], Probe::#probe{}, PR::#probe_return{}) ->
    {ok, NewInspectSates::[term()], NewProbe::#probe{}}.
% @doc
% Used by the monitor_probe module to inspect a probe return via a list
% of inspectors.
% @end
inspect_all(States, Probe, PR) ->
    OrigProbe   = ModifiedProbe = Probe,
    NewIStates  = [],
    inspect(States, NewIStates, PR, OrigProbe, ModifiedProbe).
inspect([], NewInspectStates, _, _, ModifiedProbe) ->
    {ok, NewInspectStates, ModifiedProbe};
inspect([IState|IStates], NIStates, PR, OP, MP) ->
    {Mod, State} = IState,
    {ok, NState, MProbe} = Mod:inspect(State, PR, OP, MP),
    NewNIStates = lists:keystore(Mod, 1, NIStates, {Mod, NState}),
    inspect(IStates, NewNIStates, PR, OP, MProbe).

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
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with Enms. If not, see <http://www.gnu.org/licenses/>.
-module(bmonitor_inspector_property_set).
-behaviour(beha_monitor_inspector).
-include("../../include/monitor.hrl").


-export([
    init/3,
    inspect/4,
    info/0
]).

-record(state, {
    binds
}).

info() ->
    Info =
"This simple inspector set the value of defined key in the probe property
record. The key must appear in the configuration list, and be returned by
the probe in the #probe_return.key_vals list to be set.
If the key did not appear in the #probe_return.key_vals list, the inspector
do nothing (if key_val exist, it will not be modified).",
    {ok, Info}.

init(Conf, _Target, _Probe) ->
    State = #state{binds = Conf},
    {ok, State}.

inspect(State, ProbeReturn, _OrigProbe, ModifiedProbe) ->
    Properties  = ModifiedProbe#probe.properties,
    ProbeRKV    = ProbeReturn#probe_return.key_vals,
    Binds       = State#state.binds,

    {ok, NewProperties} = update_properties(Properties, ProbeRKV, Binds),

    NewProbe    = ModifiedProbe#probe{properties = NewProperties},
    {ok, State, NewProbe}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTILS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @private
update_properties(Properties, _, []) ->
    {ok, Properties};
update_properties(Properties, ProbeRKV, [B|Binds]) ->
    case lists:keyfind(B, 1, ProbeRKV) of
        false ->
            update_properties(Properties, ProbeRKV, Binds);
        Value -> 
            NewProperties = lists:keystore(B, 1, Properties, Value),
            update_properties(NewProperties, ProbeRKV, Binds)
    end.

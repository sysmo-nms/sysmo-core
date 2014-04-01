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
% @doc
% This simple inspector set the value of defined key in the probe property
% record. The key must appear in the configuration list, and be returned by
% the probe in the #probe_return.key_vals list to be set.
% If the key did not appear in the #probe_return.key_vals list, the inspector
% do nothing (if key_val exist, it will not be modified).
% @end
-module(btracker_inspector_property_set).
-behaviour(beha_tracker_inspector).
-include("../../include/tracker.hrl").


-export([
    init/2,
    inspect/3,
    info/0
]).

info() ->
    [].

init(Conf, #ps_state{inspectors_state=IState} = ProbeServerState) ->
    IConf = lists:keystore(?MODULE, 1, IState, {?MODULE, Conf}),
    {ok, ProbeServerState#ps_state{inspectors_state = IConf}}.

% @end
inspect(_InitialP,
            #ps_state{
                probe = #probe{properties = Properties} = Probe,
                inspectors_state = IConf
            } = ProbeServerState,
            #probe_return{
                key_vals = KeyVals
            }) ->
    {?MODULE, Binds} = lists:keyfind(?MODULE, 1, IConf),
    NewProp = lists:foldl(fun(Key, Acc) ->
        case lists:keyfind(Key, 1, KeyVals) of
            false -> 
                Acc;
            Value ->
                lists:keystore(Key, 1, Acc, Value)
        end
    end, Properties, Binds),
    NewProbe = Probe#probe{properties = NewProp},
    {ok, ProbeServerState#ps_state{probe = NewProbe}}.

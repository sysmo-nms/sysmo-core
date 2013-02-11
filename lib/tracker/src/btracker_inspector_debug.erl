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
-module(btracker_inspector_debug).
-behaviour(beha_tracker_inspector).
-include("../include/tracker.hrl").
-export([
    init/2,
    inspect/3
]).

-spec init([any()], #probe_server_state{}) -> {ok, #probe_server_state{}}.
% @doc 
% Called by a probe starting to initalise the #probe_state.inspector_state
% if needed.
% @end
init(_C, #probe_server_state{inspectors_state = IState} = S) ->
    io:format("init inspect~n"),
    {ok, S#probe_server_state{inspectors_state = [more | IState]}}.

-spec inspect(
        Orig::#probe_server_state{},
        Modifed::#probe_server_state{},
        Msg::tuple()) -> 
    {ok, Other::#probe_server_state{}}.
% @doc
% Called by the probe each time an event occur.
% @end
inspect(_S1, S2, _Msg) ->
    io:format("inspect~n"),
    {ok, S2}.


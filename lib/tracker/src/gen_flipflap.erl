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
% @doc
% A module implementing this behaviour can be set used as flipflap detection
% module.
% <p>It is used from a "tracker_probe" server giving his #probe_server_state{} 
% as argument. The return include a possibly modified #probe_server_state{}.
% </p>
% <p>
%   A gen_flipflap module can keep his state in the 
%   <em>#probe_server_state.flipflap_state</em> record entry.
% <br></br><b>Be aware that
%   puting a large amount of data here can have effect on the target_probe 
%   server.</b>
% </p>
% @end
-module(gen_flipflap).
-include("../include/tracker.hrl").

-export([behaviour_info/1]).

% exported here for documentation only:
-export([init/1, inspect/1]).

% @private
behaviour_info(callbacks) ->
    [
        {init, 1},
        {inspect, 1}
    ];

behaviour_info(_) ->
    undefined.

-spec init(ProbeServerState::#probe_server_state{}) 
        -> {ok, ProbeServerState::#probe_server_state{}}.
% @doc
% Called at the target_probe:init/1 phase.
% @end
init(ProbeServerState) -> {ok, ProbeServerState}.

-spec inspect(ProbeServerState::#probe_server_state{}) 
        -> {ok, ProbeServerState::#probe_server_state{}}.
% @doc
% Called each time a message responce from the probe fun is received.
% @end
inspect(ProbeServerState) -> {ok, ProbeServerState}.




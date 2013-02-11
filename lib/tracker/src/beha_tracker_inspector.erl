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
% A module implementing this behaviour can be used as inspector detection
% module.
% <p>It is used from a "tracker_probe" server giving his #probe_server_state{}
% and last message as argument each time a child probe return. 
% The return include a possibly modified #probe_server_state{}.
% It have a full write access to the probe state. It can be used to detect
% flip flap, shaded devices (unreachable because an intermediary is down), 
% both or other.
% </p>
% <h2>Usage</h2>
% <p>
%   A gen_inspector module will mostly be used to prevent a probe to send
%   what can be considered false positive, by continuously incrementing the
%   #probe_server_state.timeout_current count for example. 
%   He can also send custom alerts using tracker_probe:notify/x function wich 
%   will in turn send the alert to the proper tracker_target_channel.<br></br>
%   Message send using this function must be understendable by 
%   tracker_target_channel and referenced in the documentation of
%   <b>tracker_probe:notify/x</b>.
% </p>
% <p>
% A probe can be configured to use one or more inspectors as defined in the
% #probe.inspectors record. A #probe.inspectors record is a list of modules.
% Each will be called in the order defined in the list.
% </p>
% <p>
%   Each gen_inspector has a chance to configure himself using the init/2 
%   function called at the probe initialisation. The arguments are 
%   {#probe_server_state{}, Conf::[any()]} where Conf is a list of arguments
%   defined in the #probe.inspectors record. At this stage, each inspectors
%   shold only add his state if needed in the 
%   #probe_server_state.inspectors_state record.
%   <p>
%   Each inspectors will be called in the order of the list, and return a 
%   possibly updated #probe_server_state{} wich will be given to the next.
%   </p>
%   <p>
%   <b>Note that the #probe_server_state.inspetors_state is shared among 
%   inspectors. Thus it is recommanded to store data in a tuple with a 
%   specific key. For example, using the gen_inspector module name as key.</b>
%   </p>
% </p>
% <p>
% All gen_inspector:inspect/3 must accept:
% <ul>
%  <li>Original::#probe_server_state{}</li>
%  <li>Modified::#probe_server_state{}</li>
%  <li>LastProbeReturn::tuple() as defined in the gen_probe module.</li>
% </ul>
% inspect/3 must then return {ok, {Original#probe_server_state{}}}. The return
% will be send to the next gen_inspector as the
% Modified::#probe_server_state{}.
% </p>
% <p>
% Note that for the first module called with inspect(State1, State2, Msg), 
% State1 will allways equal State2.
% </p>
% <h2>Configuration</h2>
% <p>
% <ul>
%   <li>
%       The gen_inspector list is defined in the #probe.inspectors record.
%   </li>
%   <li>
%       Then #probe{} record is part of the 
%       <em>#target.probes :: [#probe]</em>.
%   </li>
%   <li>The #traget{} record is the tuple stored in the module <em>
%   tracker_target_store :: module()</em>. This is were 
%   <em>tracker_target_channel :: module()</em> get his configuration.
%   </li>
% </ul>
% TODO API for modifying these settings. Actualy only modifiable using the 
% <em>tracker_target_store :: module()</em> API.
% </p>
% @end
-module(beha_tracker_inspector).
-include("../include/tracker.hrl").

-export([behaviour_info/1]).

% exported here for documentation only:
-export([
    init/2, 
    inspect/3
]).

% @private
behaviour_info(callbacks) ->
    [
        {init, 2},
        {inspect, 3}
    ];

behaviour_info(_) ->
    undefined.

-spec init(Conf::[any()], Orig::#probe_server_state{}) 
        -> {ok, New::#probe_server_state{}}.
% @doc
% Called at the target_probe:init/1 phase.
% @end
init(_ConfList, ProbeServerState) -> {ok, ProbeServerState}.

-spec inspect(
        Original::#probe_server_state{}, 
        PossiblyModified::#probe_server_state{},
        Msg::tuple()) -> 
    {ok, AnotherPossiblyModified::#probe_server_state{}}.
% @doc
% Called each time a message responce from the probe fun is received.
% @end
inspect(_OrigProbeServerState, ModifiedProbeServerState, _Msg) ->
    {ok, ModifiedProbeServerState}.


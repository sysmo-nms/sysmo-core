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
% A module implementing this behaviour can be used as a probe module.
% <p>
% It used from a tracker_probe module and executed every 
% (#probe.step seconds + process_time).
% </p>
% <h1>Probe type</h1>
% <p>
% Probes are of 3 general types. One probe can be any or all of them:
%   - "set", set properties of the target depending of a return,
%   - "get", retreive some value or generate it (reply latency),
%   - "status", move the status of a probe,
% </p>
% <h1>Special probes</h1>
% <p>
% Two spectial probes:
%   - "nagios", nagios compatible plugins wich is of type "status" and "get",
%   - "snmp", wich can be of any type "set", "get" and "status".
% </p>
% @end
-module(beha_tracker_probe).
-include("../include/tracker.hrl").

-export([behaviour_info/1]).

% export here for documentation only:
-export([
    exec/1,
    info/0]).

% @private
behaviour_info(callbacks) ->
    [
        {exec, 1},
        {info, 0}
    ];

behaviour_info(_) ->
    undefined.

-spec exec({TargetRecord::#target{}, ProbeRecord::#probe{}}) -> 
    {ok, Val::integer()} | {error, Error::any()} | timeout.
% @doc
% The return from this function will trigger another probe execution after a
% delay defined by the #probe.step entry.
% <em>exec</em> will then be called (#probe.step + (time for exec to return))
% time.
% @end
exec(_) -> {ok, 1}.

-spec info() -> {ok, string()}.
% @doc
% Called by the tracker_master_channel for presentation of the module to the
% client. Must include every aspect of the probe and special configuration
% parameters explanations.
% @end
info() -> {ok, ""}.

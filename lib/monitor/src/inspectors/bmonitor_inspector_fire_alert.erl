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
% The most simple and mandatory inspector. It set re return status of the
% ProbeServer to the return status of the probe return.
% @end
-module(bmonitor_inspector_fire_alert).
-behaviour(beha_monitor_inspector).
-include("include/monitor.hrl").


-export([
    info/0,
    init/3,
    inspect/4
]).

info() ->
    Info = 
    "Fire an alert on CRITICAL or WARNING if the parents are in an OK state
    newer than us. It may take two probes round trip to send an alert then.
    If parent state are not newer, the status is set to UNKNOWN",
    {ok, Info}.

init(_Conf, _Target, _Probe) ->
    {ok, no_state}.

inspect(State, _ProbeReturn, _OrigProbe, ModifiedProbe) ->
    {ok, State, ModifiedProbe}.

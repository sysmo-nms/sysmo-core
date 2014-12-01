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
% It used from a monitor_probe module and executed to trigger a check.
% </p>
% @end

-module(monitor_exec).
-include("include/monitor.hrl").

-callback init(Probe::#probe{}) ->
    {ok, State::any()}.
% @doc
% Called at initialization stage. Must return a State value wich
% will be gived as argument to the exec/2 callback.
% @end

-callback exec(State::any()) -> 
    {ok, State::any(), ProbeReturn::#probe_return{}}.
% @doc
% Called when a check is needed by the probe server. State is the return value
% of the init/2 callback. Must return a possibly modified state, and a probe
% return wich will be evaluated by inspectors and loggers modules.
% @end


-callback info() ->
    {ok, ProbeInfo::string()}.
% @doc
% Called by the monitor_master for presentation of the module to the
% client. Must include every aspect of the probe and special configuration
% parameters explanations.
% @end

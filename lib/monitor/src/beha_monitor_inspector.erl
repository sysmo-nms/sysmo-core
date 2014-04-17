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
-module(beha_monitor_inspector).
-include("include/monitor.hrl").

-callback info() -> {ok, Info::string()}.
% @doc
% Must return an Info string, wich explain what does the inspector.
% @end
-callback init(Conf::[any()], Target::#target{}, Probe::#probe{}) -> 
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

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
% The module implementing this behaviour is used by a monitor_probe
% to store values returned by the probes.
% @end
-module(beha_monitor_logger).
-include("include/monitor.hrl").

-callback log_init(Conf::any(), Target::#target{}, Probe::#probe{}) ->
    {ok, State::any()}.
% @doc
% Is called at initialisation stage. Must return a State wich will be used
% as first argument of the log/2 callback.
% @end


-callback log(State::any(), ProbeReturn::#probe_return{})  ->
    {ok, State::any()}              |
    {ok, State::any(), Pdu::tuple}.
% @doc
% Called each time a message responce from the probe fun is received.
% Is it to this module to be sure that a log action is not pending when
% a ?MODULE:dump/2 occur using locks or other.
% This function is called from the channel. The channel can call dump/2
% after. It is to the module to define when a log is pending before
% sending a dump using a gen_server or other lock method.
% Must return {ok, State} when State is a possibly modified State value,
% or {ok, State, Pdu} when Pdu will be sent to registered clients.
% @end

-callback dump(State::any()) -> 
    {ok,        Pdu::tuple(), State::any()}   | 
    {ignore,    State::any()}.
% @doc
% Called by a monitor_probe on a subscribe request by a client. Must
% return a binary form of the data logged or ignore if there is no need.
% For synchronisation, the monitor_probe server will wait for a
% a responce. This function MUST return before some kind of timeout or it will
% indefinitely block the probe server.
% State return is a possibly modified State, Pdu is a tuple message 
% wich will be encoded as is.
% @end

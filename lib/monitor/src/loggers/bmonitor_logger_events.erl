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
% The module implementing this behaviour is used by a monitor_target_channel
% to store values returned by the probes.
% @end
-module(bmonitor_logger_events).
-behaviour(beha_monitor_logger).
-include("include/monitor.hrl").

-export([
    init/3,
    log/2,
    dump/1
]).

-define(EVENT_SERVER, monitor_logger_events).

-record(state, {
    table_name,
    target_id
}).

init(_Conf, Target, Probe) ->
    TableName   = Probe#probe.name,
    TargetId    = Target#target.id,
    ok          = gen_server:call(?EVENT_SERVER, {init, TableName}),
    State       = #state{table_name     = TableName},
    State2      = State#state{target_id = TargetId},
    {ok, State2}.

log(State, ProbeReturn) ->
    TableName = State#state.table_name,
    case gen_server:call(?EVENT_SERVER, {log, TableName, ProbeReturn}) of
        ok          -> {ok, State};
        {ok, Pdu}   -> {ok, Pdu, State}
    end.

dump(State) ->
    TableName   = State#state.table_name,
    TargetId    = State#state.target_id,
    {ok, Pdu}   = gen_server:call(?EVENT_SERVER, {dump, TableName, TargetId}),
    {ok, Pdu, State}.

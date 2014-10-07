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
% This module log data to an rrd database. Data must exist in the
% #probe_return.key_val record.
% To work a valid #rrd_def record must be givent at init conf input.
% @end
-module(bmonitor_logger_rrd2).
-behaviour(beha_monitor_logger).
-include("include/monitor.hrl").

-export([
    init/3,
    log/2,
    dump/1
]).

init(_Conf, _Target, _Probe) ->
    io:format("init ~p~n",[?MODULE]),
    {ok, nostate}.

log(State, _ProbeReturn) ->
    io:format("log ~p~n",[?MODULE]),
    {ok, State}.


dump(State) ->
    io:format("dump ~p~n",[?MODULE]),
    {ignore, State}.

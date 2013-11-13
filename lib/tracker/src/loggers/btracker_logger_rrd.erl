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
-module(btracker_logger_rrd).
-behaviour(beha_tracker_logger).
-include("../../include/tracker.hrl").

-export([
    init/2,
    log/2,
    dump/1
]).

init(Cfg, #probe_server_state{
        target          = #target{directory = Dir},
        probe           = #probe{name = Name},
        loggers_state   = _LoggersState} = ProbeSrvState) ->
    RrdFile = generate_filename(Dir, Name),
    case file:read_file_info(RrdFile) of
        {ok, _}         ->
            ok;
        {error, enoent} ->
            {create, String} = lists:keyfind(create, 1, Cfg),
            RrdCommand = re:replace(String, "<FILE>", RrdFile, 
                [{return, list}]),
            tlogger_rrd:exec(RrdCommand)
    end,
    {ok, ProbeSrvState}.

log(_, _) ->
    ok.

dump(_) ->
    io:format("dumpfromrrd~n"),
    ignore.

generate_filename(Dir, Name) ->
    FileName    = io_lib:format("~s.rrd", [Name]),
    filename:absname_join(Dir, FileName).

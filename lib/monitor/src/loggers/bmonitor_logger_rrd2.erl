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


init(Conf, Target, _Probe) ->
    ConfType = proplists:get_value(type, Conf),
    TargetDir = Target#target.directory,
    case ConfType of
        snmp_table ->
            % the return will be from a walk_table method
            % get the create rrd string
            RrdCreateStr    = proplists:get_value(rrd_create, Conf),
            % get the list of index to file bind. Index is the second element
            % of a table element returned by the walk_table method. It must
            % uniquely identify the table row.
            TIndexToRRDFile = proplists:get_value(table_index_to_rrd_file,Conf),
            snmp_table_init_rrd_files(TargetDir, RrdCreateStr, TIndexToRRDFile),
            ok;
        _ ->
            ok
    end,

    io:format("initttttttttttttt ~p ~p~n",[?MODULE, Conf]),
    {ok, nostate}.

% @doc
% Create rrdfiles for return sent from a walk_table method.
% @end
snmp_table_init_rrd_files(TargetDir, RrdCreateStr, IndexToFile) ->
    {ok, Rx} = re:compile("<FILE>"),
    Accum    = [],
    RrdCreate = snmp_table_build_create(
        TargetDir, RrdCreateStr, IndexToFile, Rx, Accum),
    io:format("will send ~p to rrdtool ~n", [RrdCreate]).

snmp_table_build_create(_,_,[],_,Acc) -> Acc;
snmp_table_build_create(TDir,RrdCreate,[{_,File}|T],Rx,Acc) ->
    FilePath = filename:join(TDir, File),
    RrdCmd = re:replace(RrdCreate, Rx, FilePath, [{return, list}]),
    snmp_table_build_create(TDir, RrdCreate, T, Rx, [RrdCmd|Acc]).


log(State, _ProbeReturn) ->
    io:format("log ~p ~p~n",[?MODULE, _ProbeReturn]),
    {ok, State}.


dump(State) ->
    io:format("dump ~p~n",[?MODULE]),
    {ignore, State}.

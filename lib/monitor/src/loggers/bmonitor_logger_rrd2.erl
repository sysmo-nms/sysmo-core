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
    log_init/3,
    log/2,
    dump/1
]).

-record(state, {
    type,
    rrd_update,
    row_index_to_tpl,
    row_index_to_file,
    %% for dump
    target_id,
    probe_name,
    dump_dir
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% INIT (rrdcreate) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_init(Conf, Target, Probe) ->
    ConfType        = proplists:get_value(type, Conf),
    {ok, DumpDir}   = application:get_env(supercast, data_sync_dir),
    case ConfType of
        snmp_table ->
            % the return will be from a walk_table method
            % get the create rrd string
            RrdCreateStr    = proplists:get_value(rrd_create, Conf),
            % get the list of index to file bind. Index is the second element
            % of a table element returned by the walk_table method. It must
            % uniquely identify the table row.
            TargetDir       = Target#target.directory,
            IndexesRrd      = proplists:get_value(row_index_to_rrd_file, Conf),
            IndexesRrdPaths = build_rrd_file_paths(TargetDir, IndexesRrd),
            snmp_table_init_rrd_files(RrdCreateStr, IndexesRrdPaths),
 
            IndexesTpl = proplists:get_value(row_index_pos_to_rrd_template, Conf),
            RrdUpdate  = proplists:get_value(rrd_update, Conf),

            {ok,
                #state{
                    type                = ConfType,
                    rrd_update          = RrdUpdate,
                    row_index_to_file   = IndexesRrdPaths,
                    row_index_to_tpl    = IndexesTpl,
                    target_id           = Target#target.id,
                    probe_name          = Probe#probe.name,
                    dump_dir            = DumpDir
                }
            };
        _ ->
            {ok, nostate}
    end.

build_rrd_file_paths(TargetDir, Indexes) ->
    build_rrd_file_paths(TargetDir, Indexes, []).
build_rrd_file_paths(_, [], Acc) -> Acc;
build_rrd_file_paths(TargetDir, [{Id, File}|Rest], Acc) ->
    FPath = filename:join(TargetDir, File),
    build_rrd_file_paths(TargetDir, Rest, [{Id,FPath}|Acc]).

% @doc
% Create rrdfiles for return sent from a walk_table method.
% @end
snmp_table_init_rrd_files(RrdCreateStr, IndexToFile) ->
    RrdCreate = snmp_table_build_create(RrdCreateStr, IndexToFile, []),
    lists:foreach(fun(X) -> errd:create(X) end, RrdCreate).

snmp_table_build_create(_, [], Acc) -> Acc;
snmp_table_build_create(RrdCreate,[{_,File}|T],Acc) ->
    RrdCmd = lists:concat([File, RrdCreate]),
    snmp_table_build_create(RrdCreate, T, [RrdCmd|Acc]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% LOG (rrdupdate) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log(State, #probe_return{reply_tuple = ignore} = _ProbeReturn) ->
    ClientUp = [{I, ""} || {I,_} <- State#state.row_index_to_file],
    Pdu = build_rrd_event(State, ClientUp),
    {ok, Pdu, State};

log(State, #probe_return{reply_tuple = Rpl, timestamp = Ts} = _ProbeReturn) ->

    RIndexFile = State#state.row_index_to_file,
    RIndexTpl  = State#state.row_index_to_tpl,
    RrdUpdate  = State#state.rrd_update,
    {ok, ClientUp} = rrd_update(RIndexFile, RIndexTpl, RrdUpdate, Rpl, Ts),


    Pdu = build_rrd_event(State,ClientUp),

    {ok, Pdu, State}.

build_rrd_event(State, ClientUp) ->
    TId         = atom_to_list(State#state.target_id),
    ProbeName   = atom_to_list(State#state.probe_name),
    {modMonitorPDU,
        {fromServer,
            {loggerRrdEvent,
                {'LoggerRrdEvent',
                    TId,
                    ProbeName,
                    [{'LoggerRrdUpdate', I, Up} || {I,Up} <- ClientUp]
                }
            }
        }
    }.

rrd_update(RindexFile, RIndexTpl, RrdUpdate, Rpl, Ts) ->
    rrd_update(RindexFile, RIndexTpl, RrdUpdate, Rpl, Ts, []).
rrd_update([],_,_,_,_, Acc) -> {ok, Acc};
rrd_update([{Index, File}|Tail], RIndexTpl, RrdUpdate, Rpl, Ts, Acc) ->
    {value, Row, Rpl2} = lists:keytake(Index, 2, Rpl),
    TplString = rrd_update_build_tpl(RIndexTpl, Row, Ts),
    RrdCmd = lists:concat([File, " ", RrdUpdate, " ", TplString]),
    errd:update(RrdCmd),
    rrd_update(Tail, RIndexTpl, RrdUpdate, Rpl2, Ts, [{Index,TplString}|Acc]).

rrd_update_build_tpl([], _, Acc) -> Acc;
rrd_update_build_tpl([I|T], Row, Acc) ->
    Val = erlang:element(I, Row),
    rrd_update_build_tpl(T,Row, lists:concat([Acc, ":", Val])).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% DUMP (rrddump) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dump(#state{row_index_to_file = RI, dump_dir = DDir} = State) ->
    Dir = lists:concat(["tmp-", generate_tmp_dir()]),
    Dir2 = filename:join(DDir, Dir),
    ok = file:make_dir(Dir2),
    IndexToFile = dump_file(RI, Dir2),
    Pdu         = build_dump(State, IndexToFile, Dir),
    {ok, {pdu, Pdu}, State}.
    %{ignore, State}.

build_dump(State, FilePaths, Dir) ->
    TId         = atom_to_list(State#state.target_id),
    ProbeName   = atom_to_list(State#state.probe_name),
    ProbeModule = atom_to_list(?MODULE),
    {modMonitorPDU,
        {fromServer,
            {loggerRrdDump,
                {'LoggerRrdDump',
                    TId,
                    ProbeName,
                    ProbeModule,
                    [{'LoggerRrdIdToFile', I, F} || {I,F} <- FilePaths],
                    Dir
                }
            }
        }
    }.

dump_file(F, InDir) ->
    dump_file(F,InDir, []).
dump_file([], _, Acc) -> Acc;
dump_file([ {I,F} | T], InDir, Acc) ->
    XmlFile = lists:concat([I, ".xml"]),
    XmlFilePath = filename:join(InDir, XmlFile),
    errd:dump(F, XmlFilePath),
    dump_file(T, InDir, [{I,XmlFile}|Acc]).

generate_tmp_dir() ->
    {_, Sec, Micro} = os:timestamp(),
    Microsec = Sec * 1000000 + Micro,
    Microsec.

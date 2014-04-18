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
-module(bmonitor_logger_rrd).
-behaviour(beha_monitor_logger).
-include("include/monitor.hrl").

-export([
    init/3,
    log/2,
    dump/1
]).

-record(state, {
    rrds,
    target_id,
    probe_id
}).

init(Conf, Target, Probe) ->
    ?LOG({init_logger_rrd, Conf}),
    TargetId    = Target#target.id,
    ProbeId     = Probe#probe.name,
    Dir         = Target#target.directory,
    {ok, Rrds}  = update_rrd_record(Conf, Dir),
    ok          = create_file_if_needed(Rrds),
    State       = #state{
        rrds        = Rrds,
        target_id   = TargetId,
        probe_id    = ProbeId
    },
    {ok, State}.

log(State, ProbeReturn) ->
    Kv      = ProbeReturn#probe_return.key_vals,
    Rrds    = State#state.rrds,
    log_rrds(Rrds, Kv),
    {ok, State}.


dump(State) ->
    Rrds        = State#state.rrds,
    TargetId    = State#state.target_id,
    ProbeId     = State#state.probe_id,
    Pdu         = pdu('rrdProbeDump', {TargetId, ProbeId, Rrds}),
    {ok, Pdu, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTILS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pdu('rrdProbeDump', {TargetId, ProbeId, RrdConfigs}) ->
    RrdFileDumps = [{'RrdFileDump', FileId, monitor_logger_rrd:dump(File)} ||
        #rrd_config{file_path = File, file = FileId} <- RrdConfigs
    ],
    {modMonitorPDU,
        {fromServer,
            {rrdProbeDump,
                {'RrdProbeDump',
                    atom_to_list(TargetId),
                    atom_to_list(ProbeId),
                    atom_to_list(?MODULE),
                    RrdFileDumps }}}}.
    
log_rrds([], _) -> ok;
log_rrds([RrdConf|Rrds], Kv) ->
    UpdateString = RrdConf#rrd_config.update,
    ReBinds      = RrdConf#rrd_config.update_regexps,
    UpdateComm   = generate_update_string(Kv, UpdateString, ReBinds),
    rrd_exec(UpdateComm),
    log_rrds(Rrds, Kv).

rrd_exec(String) ->
    case monitor_logger_rrd:exec(String) of
        {ok, _} -> ok;
        {error, Reply} ->
            error_logger:info_msg(
                "logger_rrd_error, ~p, ~p, ~p:~p }", [Reply, String, ?MODULE, ?LINE]
            ),
            ok
    end.

update_rrd_record(Rrds, Dir) -> 
    NewRecords  = [],
    update_rrd_record(Rrds, Dir, NewRecords).
update_rrd_record([], _Dir, NewRecords) ->
    {ok, NewRecords};
update_rrd_record([Rrd|Rrds], Dir, NewRrds)   ->
    RrdFile      = Rrd#rrd_config.file,
    RrdUpdate    = Rrd#rrd_config.update,
    RrdBinds     = Rrd#rrd_config.binds,
    RrdFilePath  = generate_filename(Dir, RrdFile),

    RrdFilePath2 = if_win(RrdFilePath),
    RrdUpdate2   = re:replace(
        RrdUpdate, "<FILE>", RrdFilePath2, [{return, list}]),
    RrdRe        = compile_re(RrdBinds),

    NewRrd       = Rrd#rrd_config{
        file_path       = RrdFilePath2,
        update          = RrdUpdate2,
        update_regexps  = RrdRe
    },
    update_rrd_record(Rrds, Dir, [NewRrd|NewRrds]).
    
create_file_if_needed([])         -> ok;
create_file_if_needed([Rrd|Rrds]) ->
    FilePath    = Rrd#rrd_config.file_path,
    case file:read_file_info(FilePath) of
        {ok, _}         -> ok;
        {error, enoent} ->
            CreateString = Rrd#rrd_config.create,
            RrdCommand   = re:replace(
                CreateString, "<FILE>", FilePath, [{return, list}]),
            monitor_logger_rrd:exec(RrdCommand)
    end,
    create_file_if_needed(Rrds).

generate_filename(Dir, Name) ->
    FileName    = io_lib:format("~s.rrd", [Name]),
    filename:absname_join(Dir, FileName).

compile_re([]) ->
    [];
compile_re(L) ->
    compile_re(L, []).
compile_re([], Return) ->
    Return;
compile_re([{Ret, Macro} | L], Return) ->
    {ok, Re} = re:compile(Macro),
    compile_re(L, [{Ret, Re} | Return]).

generate_update_string(_, String, []) ->
    String;
generate_update_string(Kv, String, [{Key, Re} | Binds]) ->
    case lists:keyfind(Key, 1, Kv) of
        false ->
            error_logger:info_msg(
                "Unable to generate rrd udate, missing value ~p, ~p, ~p:~p", 
                    [Key, Kv, ?MODULE, ?LINE]
            ),
            [];
        {Key, Val} ->
            NewString = re:replace(
                String, Re, to_string(Val), [{return, list}]
            ),
            generate_update_string(Kv, NewString, Binds)
    end.
 
to_string(Term) when is_float(Term) ->
    float_to_list(Term, [{decimals, 0}, compact]);
to_string(Term) when is_integer(Term) ->
    integer_to_list(Term).

if_win(FileName) ->
    case os:type() of
        {unix, _} ->
            FileName;
        {win32, _} ->
            F1 = string:concat("\"", FileName),
            F2 = string:concat(F1, "\""),
            F2
    end.

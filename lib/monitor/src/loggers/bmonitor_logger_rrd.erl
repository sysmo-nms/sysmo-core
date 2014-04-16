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
-include("../../include/monitor.hrl").

-export([
    init/2,
    log/2,
    dump/1
]).

init(Cfg, #ps_state{
        target          = #target{directory = Dir},
        loggers_state   = LoggersState} = ProbeSrvState) ->

    % update #rrd_config.filePath and create the file if it does not exist.
    Cfg1 = [RrdRec#rrd_config{file_path = generate_filename(Dir, RrdFile)} || 
            #rrd_config{file = RrdFile} = RrdRec <- Cfg
    ],
    ok = lists:foreach(fun(RrdConf) ->
        case file:read_file_info(RrdConf#rrd_config.file_path) of
            {ok, _} ->
                ok;
            {error, enoent} ->
                CreateString = RrdConf#rrd_config.create,
                FileString   = if_win(RrdConf#rrd_config.file_path),
                RrdCommand = re:replace(CreateString, "<FILE>", FileString,
                    [{return, list}]),
                monitor_logger_rrd:exec(RrdCommand)
        end
    end, Cfg1),

    % update #rrd_config.update with the filePath name
    Cfg2 = [
        RrdRec#rrd_config{
            update = re:replace(Update, "<FILE>", if_win(File), [{return, list}])
        } || #rrd_config{
            update = Update, file_path = File
            } = RrdRec <- Cfg1
    ],

    % update #rrd_config.update_regexp with the compiled regexp
    Cfg3 = [
        RrdRec#rrd_config{
            update_regexps = compile_re(Binds)
        } || #rrd_config{binds = Binds} = RrdRec <- Cfg2
    ],

    % store config in the LoggersState
    LoggersState2 = lists:keystore(?MODULE, 1, LoggersState,
        {?MODULE, Cfg3}
    ),

    % generate the new ps_state
    NewProbeSrvState = ProbeSrvState#ps_state{
        loggers_state = LoggersState2
    },

    % END init
    {ok, NewProbeSrvState}.


log(#ps_state{loggers_state = LoggersState}, 
        #probe_return{key_vals = Kv}) ->
    % Config is a list of rrd_config records.
    % Kv is a list of {bind, value:int()}
    {?MODULE, Configs}   = lists:keyfind(?MODULE, 1, LoggersState),

    % foreach rrd_config file
    lists:foreach(fun(RrdConf) ->
        UpdateString = RrdConf#rrd_config.update,
        ReBinds      = RrdConf#rrd_config.update_regexps,
        UpdateCommand = generate_update_string(Kv, UpdateString, ReBinds),
        rrd_exec(UpdateCommand)
    end, Configs),
    ok.

dump(#ps_state{
        loggers_state   = LoggersState,
        target          = #target{id    = TargetId},
        probe           = #probe{name   = ProbeId}
    }) ->
    {?MODULE, Configs}   = lists:keyfind(?MODULE, 1, LoggersState),
    Pdu = pdu('rrdProbeDump', {TargetId, ProbeId, Configs}),
    Pdu.

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
    
rrd_exec(String) ->
    case monitor_logger_rrd:exec(String) of
        {ok, _} -> ok;
        {error, Reply} ->
            error_logger:info_msg(
                "logger_rrd_error, ~p, ~p, ~p:~p }", [Reply, String, ?MODULE, ?LINE]
            ),
            ok
    end.

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

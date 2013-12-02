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
        loggers_state   = LoggersState} = ProbeSrvState) ->
    RrdFile = generate_filename(Dir, Name),
    case file:read_file_info(RrdFile) of
        {ok, _}         ->
            ok;
        {error, enoent} ->
            {create, Create} = lists:keyfind(create, 1, Cfg),
            RrdCommand = re:replace(Create, "<FILE>", RrdFile, 
                [{return, list}]),
            tlogger_rrd:exec(RrdCommand)
    end,
    {update, Update} = lists:keyfind(update, 1, Cfg),
    Update2 = re:replace(Update, "<FILE>", RrdFile, [{return, list}]),
    {binds,  Binds}  = lists:keyfind(binds, 1, Cfg),
    Binds2  = compile_re(Binds),

    LS = lists:keystore(?MODULE, 1, LoggersState, 
        {?MODULE, [
                {file, RrdFile},
                {binds, Binds2},
                {update, Update2}
            ]
        }
    ),
    {ok, ProbeSrvState#probe_server_state{loggers_state = LS}}.

log(
        #probe_server_state{loggers_state = LS}, 
        #probe_return{key_vals = Kv}) ->
    {?MODULE, State}    = lists:keyfind(?MODULE, 1, LS),
    {update,  Update}   = lists:keyfind(update, 1, State),
    {binds,   Binds}    = lists:keyfind(binds,  1, State),
    String = generate_update_string(Kv, Update, Binds),
    case tlogger_rrd:exec(String) of
        ok -> ok;
        error ->
            ?LOG({logger_rrd_error, String})
    end.

dump(#probe_server_state{
        loggers_state   = LState,
        target          = #target{id    = TId},
        probe           = #probe{name   = PId}
    }) ->
    File    = get_file(LState),
    Bin     = tlogger_rrd:dump(File),
    Pdu     = pdu('probeDump', {TId, PId, Bin}),
    Pdu.

pdu('probeDump', {TId, PId, Bin}) ->
    {modTrackerPDU,
        {fromServer,
            {probeDump,
                {'ProbeDump',
                    atom_to_list(TId),
                    atom_to_list(PId),
                    atom_to_list(?MODULE),
                    Bin}}}}.


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
            ?LOG({"unable to generate rrd udate, missng value", Key, Kv}),
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

get_file(LState) ->
    {?MODULE, Conf} = lists:keyfind(?MODULE, 1, LState),
    {file,    File} = lists:keyfind(file, 1, Conf),
    File.

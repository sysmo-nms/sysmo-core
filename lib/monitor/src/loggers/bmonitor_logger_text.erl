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
-module(bmonitor_logger_text).
-behaviour(beha_monitor_logger).
-include("include/monitor.hrl").

-export([
    init/3,
    log/2,
    dump/1
]).

-record(state, {
    file_name,
    log_srv
}).

init(_Conf, Dir, Probe) ->
    LogFile     = generate_filename(Dir, Probe),
    {ok, Pid}   = monitor_logger_text_sup:start_logger(LogFile),
    {ok, #state{file_name = LogFile, log_srv = Pid}}.

log(State, ProbeReturn) ->
    LogSrv      = State#state.log_srv,
    Msg         = ProbeReturn#probe_return.original_reply,
    T           = ProbeReturn#probe_return.timestamp,
    EncodedMsg  = list_to_binary(io_lib:format("~p>>> ~s", [T, Msg])),
    monitor_logger_text:log(LogSrv, EncodedMsg),
    ok.

dump(#ps_state{
        loggers_state   = LState,
        target          = #target{id = TId},
        probe           = #probe{name = PId}
    }) ->
    LogSrv  = get_key(log_srv, LState),
    Bin     = monitor_logger_text:dump(LogSrv),
    Pdu     = pdu('probeDump', {TId, PId, Bin}),
    Pdu.

pdu('probeDump', {TargetId, ProbeId, Binary}) ->
    {modMonitorPDU,
        {fromServer,
            {probeDump,
                {'ProbeDump',
                    atom_to_list(TargetId),
                    atom_to_list(ProbeId),
                    atom_to_list(?MODULE),
                    Binary}}}}.

generate_filename(TargetDir, Probe) ->
    ProbeName   = Probe#probe.name,
    FileName    = io_lib:format("~s.txt", [ProbeName]),
    filename:absname_join(TargetDir, FileName).

get_key(Key, TupleList) ->
    {?MODULE, Conf} = lists:keyfind(?MODULE, 1, TupleList),
    {Key, Value}    = lists:keyfind(Key, 1, Conf),
    Value.

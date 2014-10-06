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
    log_srv,
    target_id,
    probe_name
}).

init(_Conf, Target, Probe) ->
    Dir         = Target#target.directory,
    TargetId    = Target#target.id,
    ProbeName   = Probe#probe.name,
    LogFile     = generate_filename(Dir, ProbeName),
    {ok, Pid}   = text_logger_sup:start_logger(LogFile),
    State       = #state{
        file_name   = LogFile,
        log_srv     = Pid,
        target_id   = TargetId,
        probe_name  = ProbeName
    },
    {ok, State}.

log(State, ProbeReturn) ->
    LogSrv      = State#state.log_srv,
    Msg         = ProbeReturn#probe_return.original_reply,
    T           = ProbeReturn#probe_return.timestamp,
    EncodedMsg  = list_to_binary(io_lib:format("~p>>> ~s", [T, Msg])),
    text_logger:log(LogSrv, EncodedMsg),
    {ok, State}.

dump(State) ->
    LogSrv      = State#state.log_srv,
    TargetId    = State#state.target_id,
    ProbeName   = State#state.probe_name,
    {ok, Bin}   = text_logger:dump(LogSrv),
    Pdu         = pdu('probeDump', {TargetId, ProbeName, Bin}),
    {ok, {pdu, Pdu}, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTILS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pdu('probeDump', {TargetId, ProbeName, Binary}) ->
    {modMonitorPDU,
        {fromServer,
            {probeDump,
                {'ProbeDump',
                    atom_to_list(TargetId),
                    atom_to_list(ProbeName),
                    atom_to_list(?MODULE),
                    Binary}}}}.

generate_filename(TargetDir, ProbeName) ->
    FileName    = io_lib:format("~s.txt", [ProbeName]),
    filename:absname_join(TargetDir, FileName).

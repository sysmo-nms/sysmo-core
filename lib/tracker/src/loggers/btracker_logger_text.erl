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
% The module implementing this behaviour is used by a tracker_target_channel
% to store values returned by the probes.
% @end
-module(btracker_logger_text).
-behaviour(beha_tracker_logger).
-include("../../include/tracker.hrl").

-export([
    init/2,
    log/2,
    dump/2
]).

init(_Conf, #probe_server_state{
        target          = _Target,
        probe           = _Probe,
        loggers_state   = _LoggersState} = ProbeServerState) -> 
    % TargetDir   = Target#target.directory,
    % ProbeName   = Probe#probe.name,
    % ProbeId     = integer_to_list(Probe#probe.id),
    % FileName    = io_lib:format("~s-~s.log", [ProbeName, ProbeId]),
    % LogFile     = filename:absname_join(TargetDir, FileName),

        
    % {ok, IoD} = file:open(LogFile, [append]),
    % file:close(IoD),

    % NewLoggersState =lists:keystore(?MODULE, 1, 
            % LoggersState, 
                % {?MODULE, [{file_name, LogFile}] }),
    io:format("iiiiiiiiiiiiinit~n"),
    {ok, 
        ProbeServerState
        %ProbeServerState#probe_server_state{
            %loggers_state = NewLoggersState
        %}
    }.

log(_P, _Msg) ->
    io:format("llllllllog~n"),
    ok.

% log(#probe_server_state{loggers_state = LoggersState}, Msg) ->
    % {?MODULE, Conf} = lists:keyfind(?MODULE, 1, LoggersState),
    % {_, F}          = lists:keyfind(file_name, 1, Conf),
    % EncodedMsg      = list_to_binary(io_lib:format("~p~n", [Msg])),
    % file:write_file(F, EncodedMsg, [append]),
    % ok.

dump(_ProbeServerState, _Timeout) -> 
    io:format("dddddddddddddddump~n"),
    ignore.

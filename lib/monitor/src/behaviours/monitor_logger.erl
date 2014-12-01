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
% The module implementing this behaviour is used by a monitor_probe
% to store values returned by the probes.
% @end
-module(monitor_logger).
-include("include/monitor.hrl").

-export([
    init_all/2,
    log_all/2,
    dump_all/2
]).

-callback log_init(Conf::any(), Target::#target{}, Probe::#probe{}) ->
    {ok, State::any()}.
% @doc
% Is called at initialisation stage. Must return a State wich will be used
% as first argument of the log/2 callback.
% @end


-callback log(State::any(), ProbeReturn::#probe_return{})  ->
    {ok, State::any()}              |
    {ok, State::any(), Pdu::tuple}.
% @doc
% Called each time a message responce from the probe fun is received.
% Is it to this module to be sure that a log action is not pending when
% a ?MODULE:dump/2 occur using locks or other.
% This function is called from the channel. The channel can call dump/2
% after. It is to the module to define when a log is pending before
% sending a dump using a gen_server or other lock method.
% Must return {ok, State} when State is a possibly modified State value,
% or {ok, State, Pdu} when Pdu will be sent to registered clients.
% @end

-callback dump(State::any(), CSate::tuple()) -> 
    {ok,        Pdu::tuple(), State::any()}   | 
    {ignore,    State::any()}.
% @doc
% Called by a monitor_probe on a subscribe request by a client. Must
% return a binary form of the data logged or ignore if there is no need.
% For synchronisation, the monitor_probe server will wait for a
% a responce. This function MUST return before some kind of timeout or it will
% indefinitely block the probe server.
% State return is a possibly modified State, Pdu is a tuple message 
% wich will be encoded as is.
% @end

-spec init_all(Target::#target{}, Probe::#probe{}) -> {ok, [term()]}.
% @doc
% Used by monitor_probe to init all the loggers. Return their state.
% @end
init_all(Target, Probe) ->
    Loggers         = Probe#probe.loggers,
    LoggersState    = [],
    init_loggers(Target, Probe, Loggers, LoggersState).
init_loggers(_Target, _Probe, [], LoggersState) ->
    {ok, LoggersState};
init_loggers(Target, Probe, [Logger|Loggers], LoggersState) ->
    Mod             = Logger#logger.module,
    Conf            = Logger#logger.conf,
    {ok, State}     = Mod:log_init(Conf, Target, Probe),
    LoggersState2   = lists:keystore(Mod, 1, LoggersState, {Mod, State}),
    init_loggers(Target, Probe, Loggers, LoggersState2).

-spec log_all(LoggersState::[term()], PR::#probe_return{}) ->
    {ok, Pdus::[term()], State::[term()]}.
% @doc
% Used by monitor_probe to log a probe_return. Eventualy return some PDU to
% update the subscribers of the channel.
% @end
log_all(LoggersState, ProbeReturn) ->
        LoggersStateAcc = [],
        LoggersPduAcc = [],
    log_return(LoggersState, ProbeReturn, LoggersStateAcc, LoggersPduAcc).
log_return([],  _, LoggersStateAcc,LoggersPduAcc) ->
    {ok, LoggersPduAcc, LoggersStateAcc};
log_return([Logger|LoggersState],  ProbeReturn, LoggersStateAcc, LoggersPduAcc) ->
    {Mod, State}     = Logger,
    % loggers may return a Pdu to update the client state.
    case Mod:log(State, ProbeReturn) of
        {ok, NewState}      ->
            LoggersStateAcc2 = lists:keystore(Mod,1,LoggersStateAcc, {Mod, NewState}),
            LoggersPduAcc2 = LoggersPduAcc;
        {ok, Pdu, NewState} ->
            LoggersStateAcc2 = lists:keystore(Mod,1,LoggersStateAcc, {Mod, NewState}),
            LoggersPduAcc2 = [Pdu|LoggersPduAcc]
    end,
    log_return(LoggersState, ProbeReturn, LoggersStateAcc2, LoggersPduAcc2).

-spec dump_all(LoggersState::[term()], CLientState::term()) ->
    {ok, Pdus::[term()]}.
% @doc
% Used by monitor_probe to dump all loggers for client ClientState
% @end
dump_all(LoggersState, CState) ->
    PduAcc      = [],
    NewLogState = [],
    log_dump(LoggersState, PduAcc, NewLogState, CState).
log_dump([], Pdus, LogState, _) ->
    {ok, Pdus, LogState};
log_dump([LoggerState|LState], PduAcc, NLogState, CState) ->
    {Mod, State}        = LoggerState,
    case Mod:dump(State, CState) of
        {ok, Pdu, State2} ->
            log_dump(LState, [Pdu|PduAcc], [{Mod, State2}|NLogState], CState);
        {ignore, State2} ->
            log_dump(LState, PduAcc,       [{Mod, State2}|NLogState], CState)
    end.

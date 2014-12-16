% This file is part of "Enms" (http://sourceforge.net/projects/enms/)
% Copyright (C) 2012 <Sébastien Serre sserre.bx@gmail.com>
% 
% Enms is a Network Management System aimed to manage and monitor SNMP
% target, monitor network hosts and services, provide a consistent
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
% @private
-module(monitor_commander).
-behaviour(supercast_commander).
-behaviour(gen_server).

% GEN_SERVER
-export([
    start_link/0,
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

% SUPERCAST_COMMANDER
-export([
    handle_command/2
]).

handle_command(Command, CState) ->
    {modMonitorPDU, {fromClient, CastCommand}} = Command,
    gen_server:cast(?MODULE, {CastCommand, CState}).

handle_cast({{extendedQueryFromClient,
        {_, QueryId, {createTargetQuery, {_,SysProp,Prop}}}}, CState}, S) ->
    NProp    = [{Key,Val} || {'Prop', Key, Val} <- Prop],
    NSysProp = [{Key,Val} || {'Prop', Key, Val} <- SysProp],
    TargetId = monitor:target_new(NSysProp, NProp),
    ReplyPDU = monitor_pdu:'PDU-MonitorPDU-fromServer-extendedReply'(
        QueryId, true, true, {string, TargetId}),
    supercast_channel:unicast(CState, [ReplyPDU]),
    {noreply, S};

handle_cast(R, S) ->
    error_logger:info_msg("unknown cast for command ~p ~p ~p~n", [?MODULE, ?LINE, R]),
    {noreply, S}.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([])                -> {ok, nostate}.
handle_call(_R, _F, S)  -> {noreply, S}.
handle_info(_I, S)      -> {noreply, S}.
terminate(_R, _S)       -> normal.
code_change(_O, S, _E)  -> {ok, S}.

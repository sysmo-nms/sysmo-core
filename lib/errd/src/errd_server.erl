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
-module(errd_server).
-behaviour(gen_server).
-include("include/errd.hrl").

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    start_link/3,
    exec/2
]).

-record(state, {
    port,
    timeout
}).

% @private
start_link(ServerName, Nice, Timeout) ->
    gen_server:start_link({local, ServerName}, ?MODULE, [Nice, Timeout], []).


% @doc
% Rrdtool call.
% @end
exec(ServerName, Command) ->
    io:format("execute: ~p ~p~n", [ServerName, Command]),
    gen_server:call(ServerName, {exec, string:concat(Command, "\n")}).


% @private
init([Nice, Timeout]) ->
    {ok, RrdtoolCmd} = application:get_env(errd, rrdtool),
    Spawn = build_spawn_cmd(RrdtoolCmd, Nice),
    Port = erlang:open_port({spawn, Spawn}, 
        [use_stdio, exit_status, {line, 100}]),
    {ok, #state{port=Port, timeout=Timeout}}.


% @private
handle_call({exec, Command}, _F, #state{port=Port, timeout=Timeout} = S) ->
    erlang:port_command(Port, Command),
    case get_response(Port, Timeout) of
        {timeout, PartialReply} ->
            {stop, {errd_port_timeout, self(), Timeout, PartialReply}, S};
        Reply -> 
            io:format("reply is ~p ~n", [Reply]),
            {reply, Reply, S}
    end.

% @private
handle_cast(_,S) ->
    {noreply, S}.


% @private
handle_info({'EXIT', Port, Reason}, #state{port=Port} = S) ->
    {stop, {port_terminated, Reason}, S}.

% @private
terminate({port_terminated, _R},_) ->
    ok;
terminate(_, #state{port=Port}) ->
    port_close(Port).


% @private
code_change(_,S,_) ->
    {ok, S}.

% UTILS
% @private
build_spawn_cmd(RrdtoolCmd, Nice) ->
    Os = os:type(),
    build_spawn_cmd(RrdtoolCmd, Nice, Os).

% @private
build_spawn_cmd(RrdtoolCmd, Nice, {unix, _}) ->
    NiceValue = proplists:get_value(Nice, ?NICE_UNIX),
    lists:flatten(io_lib:format("nice -n ~s ~s -", [NiceValue,RrdtoolCmd]));
build_spawn_cmd(RrdtoolCmd, Nice, {win32, _}) ->
    _NiceValue = proplists:get_value(Nice, ?NICE_WINDOWS),
    %lists:flatten(io_lib:format("cmd.exe /Q /C START /~s /B /WAIT ~s -", [NiceValue,RrdtoolCmd])).
    lists:flatten(io_lib:format("~s -", [RrdtoolCmd])).

% @private
get_response(Port, Timeout) ->
    get_response(Port, Timeout, []).
get_response(Port, Timeout, Reply) ->
    receive
        {Port, {data, {eol, "ERROR" = Line}}} ->
            %io:format("~n1 ---------------------- ~p", [Reply]),
            {error, lists:append([Reply, Line])};
        {Port, {data, {noeol, "ERROR" ++ _ = Line}}} ->     % WTF
            {error, lists:append([Reply, Line])};
        {Port, {data, {eol, "ERROR" ++ _ = Line}}} ->
            %io:format("~n2 ---------------------- ~p", [Reply]),
            {error, lists:append([Reply, Line])};
        {Port, {data, {eol, "OK" = Line}}} ->
            %io:format("~n3 ---------------------- ~p", [Reply]),
            {ok, lists:append([Reply, Line])};
        {Port, {data, {eol, "OK" ++ _ = Line}}} ->
            %io:format("~n4 ---------------------- ~p", [Reply]),
            {ok, lists:append([Reply, Line])};
        {Port, {data, {eol, Line}}} ->
            get_response(Port, lists:append([Reply, Line]))

    after Timeout -> 
            {timeout, Reply}
    end.

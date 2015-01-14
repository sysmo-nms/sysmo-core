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
-module(fcgisrv_server).
-behaviour(gen_server).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    start_link/0
]).

-record(state, {
    port
}).

% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


% @private
init([]) ->
    process_flag(trap_exit, true),
    {ok, ExtProg} = application:get_env(fcgisrv, executable),
    Port = open_port({spawn, ExtProg}, [stream, {line, 1000}]),
    {ok, #state{port=Port}}.


% @private
handle_call(_, _F, S) ->
    {noreply, S}.

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

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
-module(errd_server_call_queue).
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
    start_link/2,
    call_rrd/2
]).

-record(state, {
    rrd_server
}).

% @private
start_link(ServerName, RrdServer) ->
    gen_server:start_link({local, ServerName}, ?MODULE, RrdServer, []).


% @doc
% Rrdtool call.
% @end
call_rrd(ServerName, Command) ->
    Cmd = string:concat(Command, "\n"),
    gen_server:call(ServerName, {call_rrd, Cmd}).


% @private
init(RrdServer) ->
    {ok, #state{rrd_server=RrdServer}}.


% @private
handle_call({call_rrd, Cmd}, _F, #state{rrd_server=RrdSrv} = S) ->
    Reply = gen_server:call(RrdSrv, {exec, Cmd}),
    {reply, Reply, S}.

% @private
handle_cast(_,S) ->
    {noreply, S}.


% @private
handle_info(_, S) ->
    {stop, unknown_info, S}.

% @private
terminate(_, _) ->
    ok.


% @private
code_change(_,S,_) ->
    {ok, S}.

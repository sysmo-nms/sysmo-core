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
% @private
-module(probe).
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
    start_link/1,
    launch/1
]).

%%-------------------------------------------------------------
%% API
%%-------------------------------------------------------------
% @doc start the server.
start_link({Id, Fun}) ->
    gen_server:start_link({local, Id}, [{Fun}], []).

% after the genserver finished, the probe must be initated with this procedure
launch(Id) ->
    gen_server:call(Id, launch).

%% GEN_SERVER CALLBACKS
%%-------------------------------------------------------------
init([Conf]) ->
    {ok, Conf}.

handle_call(launch, _F, S) ->
    %spawn(Fun(self())),
    {reply, ok, S};

handle_call(R, _F, S) ->
    io:format("handle_call ~p ~p ~p ~p~n", [?MODULE, R, _F, S]),
    {noreply, S}.

handle_cast(reply_from_fun, S) ->
    %spawn(Fun(self())),
    {noreply, S}.

% OTHER
handle_info(I, S) ->
    io:format("handle_info ~p ~p ~p~n", [?MODULE, I, S]),
    {noreply, S}.

terminate(_R, _S) ->
    io:format("terminate ~p ~p ~p~n", [?MODULE, _R, _S]),
    normal.

code_change(_O, S, _E) ->
    io:format("code_change ~p ~p ~p ~p~n", [?MODULE, _O, _E, S]),
    {ok, S}.

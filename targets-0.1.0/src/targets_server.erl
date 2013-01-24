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
-module(targets_server).
-behaviour(gen_server).
-include_lib("../include/targets.hrl").

% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

% API
-export([
    start_link/1,
    dump/0
]).

-record(tserver_state, {
    db_dir
}).

% API
start_link(DbDir) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [DbDir], []).

dump() ->
    gen_server:cast(?MODULE, dump).

% CALLBACKS
init([DbDir]) ->
    {ok, #tserver_state{db_dir = DbDir}}.

handle_call(_Q, _F, S) ->
    {noreply, S}.

handle_cast(dump, S) ->
    io:format("dir is ~p~n", [S#tserver_state.db_dir]),
    {noreply, S};
handle_cast(_Q, S) ->
    {noreply, S}.

handle_info(_I,S) ->
    {noreply, S}.

terminate(_R, _S) ->
    normal.

code_change(_O, S, _E) ->
    {ok, S}.
    

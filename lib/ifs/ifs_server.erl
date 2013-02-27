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
% @doc
% @end
-module(ifs_server).
-behaviour(gen_server).
-include("../include/ifs.hrl").

-export([
    init/1,
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

-export([
    start_link/1
]).

%%-------------------------------------------------------------
%% API
%%-------------------------------------------------------------
% @private
-spec start_link(ServerConf::any()) -> {ok, pid()}.
start_link(ServerConf) ->
    io:format("server conf is ~p~n",[ServerConf]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%-------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%-------------------------------------------------------------
% @private
init([]) ->
    {ok, []}.
        
handle_call(_R, _F, S) ->
    {noreply, S}.
%%-------------------------------------------------------------
% HANDLE_CAST
%%-------------------------------------------------------------
handle_cast(_R, S) ->
    {noreply, S}.

% OTHER
% @private
handle_info(I, S) ->
    io:format("handle_info ~p ~p ~p~n", [?MODULE, I, S]),
    {noreply, S}.

% @private
terminate(_R, _S) ->
    normal.

% @private
code_change(_O, S, _E) ->
    {ok, S}.

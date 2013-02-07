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
-module(activity_logger_fd).
-behaviour(gen_server).

-export([
    start_link/1,
    log/1
]).

-export([
    init/1,
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

log(Msg) ->
     gen_server:call(?MODULE, {log, Msg}).
        

%%-------------------------------------------------------------
%% without this small server utility, random:uniform is called
%% at the same time at startup and return identical values.
%%-------------------------------------------------------------
start_link(LogFile) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [LogFile], []).

init([LogFile]) ->
    {ok, Fd} = file:open(LogFile, [append]),
    {ok, Fd}.
    
handle_call({log, Msg}, _F, S) ->
    ok = file:write(S, Msg),
    {reply, ok, S};

handle_call(_R, _F, S) ->
    {noreply, S}.

handle_cast(_R, S) ->
    {noreply, S}.

handle_info(_I, S) ->
    {noreply, S}.

terminate(_R, S) ->
    file:close(S),
    normal.

code_change(_O, S, _E) ->
    {ok, S}.
%%-------------------------------------------------------------
%% end of gen_server
%%-------------------------------------------------------------

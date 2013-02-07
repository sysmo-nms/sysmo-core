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
-module(activity_logger).
-behaviour(gen_event).

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

init(Mod) ->
    {ok, Mod}.

handle_event(Event, S) ->
    log({Event, S}),
    {ok, S}.


%% not used
handle_call(_Request, S) ->
    {ok, ok, S}.

handle_info(_Info, S) ->
    {ok, S}.

terminate(_Args, _S) ->
    ok.

code_change(_OldVsn, S, _ExtraA) ->
    {ok, S}.

log({Event, Mod}) ->
    Chars = io_lib:format("~W~n", 
                [{Mod, Event}, 9]),
    activity_logger_fd:log(Chars).


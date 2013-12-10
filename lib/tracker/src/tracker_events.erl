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
-module(tracker_events).
-behaviour(gen_event).
-include("../include/tracker.hrl").

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).


init(_Arg) ->
    {ok, state}.

handle_event(
        {probe_move, _ProbeName, _OldState, _NewState, _ProbeReturn}, State) ->
    io:format("probe_move event received ~n"),
    {ok, State};

handle_event(Msg, State) ->
    io:format("event received ~p~n", [Msg]),
    {ok, State}.

handle_call(Request, State) ->
    io:format("call received ~p~n", [Request]),
    {ok, received, State}.

handle_info(Info, State) ->
    io:format("info received ~p~n",[Info]),
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

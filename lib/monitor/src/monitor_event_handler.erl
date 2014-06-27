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
-module(monitor_event_handler).
-include("include/monitor.hrl").
-behaviour(gen_event).

% handler
-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

init(_) -> 
    {ok, nostate}.

handle_event({probe_activity, _Target, _Probe, _Return}, State) ->
    {ok, State};
handle_event({probe_info, _TargetId, _NewProbe}, State) ->
    ?LOG({probe_info, _TargetId, _NewProbe}),
    {ok, State};
handle_event({new_target, _Event}, State) ->
    {ok, State};
handle_event({create_probe, _NewTarget}, State) ->
    {ok, State};
handle_event(_, State) ->
    {ok, State}.

handle_call(Request, State) ->
    ?LOG({"handle_call", Request}),
    {ok, ok, State}.
handle_info(Info, State) ->
    ?LOG({"handle_info", Info}),
    {ok, State}.
terminate(_Arg, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

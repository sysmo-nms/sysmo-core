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
-module(monitor_sys_events).
-include("include/monitor.hrl").
-behaviour(gen_event).

% manager
-export([
    start_link/0,
    notify/1,
    subscribe/1
]).

% handler
-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).


%% manager
start_link() ->
    Ret = gen_event:start_link({local, ?MODULE}),
    ok  = gen_event:add_handler(?MODULE, ?MODULE, no_args),
    Ret.

notify(Event) ->
    gen_event:notify(?MODULE, Event).

subscribe(Handler) ->
    gen_event:add_handler(?MODULE, Handler, no_args).


%% standard handler
-record(state, {
    dir,
    file
}).

init(_) -> 
    {ok, Dir} = application:get_env(monitor, events_dir),
    case filelib:is_dir(Dir) of
        true ->     ok;
        false ->
            ok = file:make_dir(Dir)
    end,
    {ok, #state{dir = Dir}}.

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





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
-module(monitor_alerts).
-behaviour(gen_server).
-include("include/monitor.hrl").

% GEN_SERVER
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

% SRV
-export([
    start_link/0
]).

-export([
    notify/2
]).

-record(state, {
    ets_notif
}).

-record(notif, {
    probe,
    status,
    time
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

notify(Name, Status) ->
    Ts = get_ts(),
    gen_server:cast(?MODULE, {notify, Name, Status, Ts}).

%%----------------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%----------------------------------------------------------------------------
init([]) ->
    Notifs = ets:new(notif,
        [
            set,
            compressed,
            {keypos,2}
        ]
    ),
    {ok, #state{ets_notif=Notifs}}.

handle_call(_R,_F,S) ->
    {noreply, S}.

handle_cast({notify, Name, "OK", Time}, #state{ets_notif=Tid} = S) ->
    ets:insert(Tid, #notif{probe=Name,status="OK",time=Time}),
    {noreply, S};

handle_cast({notify, Name, Status, Time}, #state{ets_notif=Tid} = S) ->
    ?LOG({todo_notify, Name, Status, Time}),
    ets:insert(Tid, #notif{probe=Name,status=Status,time=Time}),
    {noreply, S};

handle_cast(_R, S) ->
    {noreply, S}.

handle_info(_I, S) ->
    {noreply, S}.

terminate(_R, _) ->
    normal.

code_change(_O, S, _E) ->
    {ok, S}.

get_ts() ->
    {Meg,Sec,_} = erlang:now(),
    1000000 * Meg + Sec.


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
-module(monitor_events).
-behaviour(gen_server).
-include("include/monitor.hrl").

% GEN_SERVER
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,
         terminate/2,code_change/3]).

-export([start_link/0]).

% API
-export([notify/2,notify_move/5,notify_init/2,select/1]).

-record(state, {
          last_notif,
          last_move,
          db_pid}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

select(_Any) ->
    % TODO select latest entries,
    % TODO select all events from probe id,
    %
    % TODO WARNING WARNING WARNING
    % SELECT from nchecks_probe synchronize must pass from this
    % gen_server to keep data consistency (SQLDb mailbox message order).
    % See snmpman,nchecks,errd4j for java calls.
    ok.

notify(Name, Status) ->
    Ts = get_ts(),
    gen_server:cast(?MODULE, {notify, Name, Status, Ts}).

notify_init(Name, Status) ->
    Ts = get_ts(),
    gen_server:cast(?MODULE, {notify_init, Name, Status, Ts}).

notify_move(Name, CheckId, Status, StatusCode, ReplyString) ->
    Ts = get_ts(),
    gen_server:cast(?MODULE, {notify_move, Name, CheckId,
                              Status, StatusCode, ReplyString, Ts}).


%%----------------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%----------------------------------------------------------------------------
init([]) ->
    Notifs   = ets:new(last_notif, [set,{keypos,2}]),
    LastMove = ets:new(last_move, [set,{keypos,2}]),
    Pid      = sysmo:get_pid(database),
    {ok, #state{last_notif=Notifs,last_move=LastMove, db_pid=Pid}}.

handle_call(_R,_F,S) ->
    {noreply, S}.

handle_cast({notify, Name, Status, Time}, #state{last_notif=Nt} = S) ->
    ets:insert(Nt, #notification{probe=Name,status=Status,time=Time}),
    {noreply, S};

% It is a move of status, insert in last_move table, and log to db.
% Trigger some actions to define if we should trigger a mail alert based on
% the dependency table. Maybe trigger some parents probe to complete informations
% When fired, use the target.properties "mailAlertL1", and erlang:send_after
% target.properties "mailAlertEscalationAfter". Cancel the timer if a status
% move to OK occur between.
% Keep a state of all this.
% Emit info for supercast.
handle_cast({notify_move, Name, CheckId, Status, StatusCode, String, Time},
            #state{last_notif=Nt,last_move=Mv,db_pid=Db} = S) ->
    Notif = #notification{
               probe=Name,
               check_id=CheckId,
               status=Status,
               status_code=StatusCode,
               time=Time,
               string=String},
    ets:insert(Nt, Notif),
    ets:insert(Mv, Notif),
    Db ! Notif,
    {noreply, S};


% called at probe startup, do not need to update db
handle_cast({notify_init, Name, Status, Time},
            #state{last_notif=Nt,last_move=Mv} = S) ->
    Notif = #notification{probe=Name,status=Status,time=Time},
    ets:insert(Nt, Notif),
    ets:insert(Mv, Notif),
    {noreply, S};


handle_cast(_R, S) -> {noreply, S}.

handle_info({escalation, _Probe}, S) -> {noreply, S};

handle_info(_I, S) -> {noreply, S}.

terminate(_R, _) -> normal.

code_change(_O, S, _E) -> {ok, S}.


get_ts() ->
    {Meg,Sec,_} = erlang:now(),
    1000000 * Meg + Sec.

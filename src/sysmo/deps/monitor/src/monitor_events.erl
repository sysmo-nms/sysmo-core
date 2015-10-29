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
-include("monitor.hrl").
-include_lib("j_server/include/eventdb.hrl").

% GEN_SERVER
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-export([start_link/0]).

% API
-export([notify/2, notify_move/8, notify_init/2,
    dump_probe_events/2, dump_latest_events/1]).

-record(state, {last_notif, last_move}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% SELECT from nchecks_probe or monitor_channel synchronize must pass from this
% gen_server to keep data consistency (SQLDb mailbox message order).
dump_probe_events(DumpPath, Probe) ->
    gen_server:call(?MODULE, {dump_probe_events, DumpPath, Probe}).
dump_latest_events(DumpPath) ->
    % IDEM
    gen_server:call(?MODULE, {dump_latest_events, DumpPath}).

notify(Name, Status) ->
    Ts = get_ts(),
    gen_server:cast(?MODULE, {notify, Name, Status, Ts}).

notify_init(Name, Status) ->
    Ts = get_ts(),
    gen_server:cast(?MODULE, {notify_init, Name, Status, Ts}).

notify_move(Name, CheckId, Descr, Status, StatusCode,
            ReplyString, BelongTo, Perm) ->
    Ts = get_ts(),
    gen_server:call(?MODULE, {notify_move, Name, CheckId, Descr,
        Status, StatusCode, ReplyString, BelongTo, Ts, Perm}).


%%----------------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%----------------------------------------------------------------------------
init([]) ->
    Notifs   = ets:new(last_notif, [set,{keypos,2}]),
    LastMove = ets:new(last_move, [set,{keypos,2}]),
    {ok, #state{last_notif=Notifs,last_move=LastMove}}.

% It is a move of status, insert in last_move table, and log to db.
% Trigger some actions to define if we should trigger a mail alert based on
% the dependency table. Maybe trigger some parents probe to complete informations
% When fired, use the target.properties "mailAlertL1", and erlang:send_after
% target.properties "mailAlertEscalationAfter". Cancel the timer if a status
% move to OK occur between.
% Keep a state of all this.
% Emit info for supercast.
handle_call({notify_move, Name, CheckId, Descr, Status,
             StatusCode, String, BelongTo, Time, Perm}, _From,
            #state{last_notif=Nt,last_move=Mv} = S) ->
    {TargetDisplay,TargetLocation,TargetContact} = get_target_infos(BelongTo),
    Notif = #notification{
        probe=Name,
        check_id=CheckId,
        status=Status,
        status_code=StatusCode,
        time=Time,
        return_string=String,
        description=Descr,
        target_display=TargetDisplay,
        target_location=TargetLocation,
        target_contact=TargetContact},
    ets:insert(Nt, Notif),
    ets:insert(Mv, Notif),
    j_server_eventdb:notify(Notif),
    Pdu = monitor_pdu:nchecksDbNotif(Notif),
    supercast_channel:emit(?MASTER_CHANNEL, {Perm, Pdu}),
    {reply, ok, S};

handle_call({dump_probe_events, DumpPath, Probe}, _F, S) ->
    Reply = j_server_eventdb:dump_probe_events(DumpPath, Probe),
    {reply, Reply, S};

handle_call({dump_latest_events, DumpPath}, _F, S) ->
    Reply = j_server_eventdb:dump_latest_events(DumpPath),
    {reply, Reply, S};

handle_call(_R,_F,S) ->
    {noreply, S}.

handle_cast({notify, Name, Status, Time}, #state{last_notif=Nt} = S) ->
    ets:insert(Nt, #notification{probe=Name,status=Status,time=Time}),
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

get_target_infos(Target) ->
    case monitor_data_master:get(target, Target) of
        [#target{properties=Props}] ->
            Host    = proplists:get_value("host",    Props),
            SysName = proplists:get_value("sysName", Props),
            Name    = proplists:get_value("name",    Props),

            DisplayName = case SysName of
                              "undefined" ->
                                  case Name of
                                      "undefined" -> Host;
                                      ""          -> Host;
                                      _           -> Name ++ "(" ++ Host ++ ")"
                                  end;
                              _ ->
                                  SysName ++ "(" ++ Host ++ ")"
                          end,

            SysLocation = proplists:get_value("sysLocation", Props, "undefined"),
            SysContact  = proplists:get_value("sysContact", Props, "undefined"),
            {DisplayName, SysLocation, SysContact};
        _ ->
            {"undefined", "undefined", "undefined"}
    end.

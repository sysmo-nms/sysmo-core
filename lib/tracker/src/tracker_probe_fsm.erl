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
-module(tracker_probe_fsm).
-behaviour(gen_fsm).
-behaviour(gen_channel).
-include("../include/tracker.hrl").

% gen_fsm
-export([
    init/1,
    handle_event/3, 
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4
]).

% gen_fsm states
-export([
    'SLEEPING'/2,
    'WAITING-REPLY'/2,
    'NEGOCIATE-WAITING-REPLY'/2,
    'NEGOCIATE-SLEEPING'/2
]).

% local api
-export([
    start_link/1,
    launch_probe/1
]).

% btracker_inspector_parent api
-export([
    if_parent_is_ok/2,
    child_query/2
]).

% gen_channel
-export([
    get_perms/1,
    sync_request/2
]).


start_link({Target, #probe{name = Name} = Probe}) ->
    gen_fsm:start_link({local, Name}, ?MODULE, [Target, Probe], []).

get_perms(PidName) ->
    gen_fsm:sync_send_all_state_event(PidName, get_perms).

sync_request(PidName, CState) ->
    gen_fsm:send_all_state_event(PidName, {sync_request, CState}).

if_parent_is_ok(PidName, Status) ->
    gen_fsm:send_all_state_event(PidName, {if_parent_is_ok, Status}).

child_query(PidName, Child) ->
    gen_fsm:send_all_state_event(PidName, {child_query, Child}).

init([Target, Probe]) ->
    S1 = #ps_state{
        target              = Target,
        probe               = Probe#probe{pid = self()},
        step                = Probe#probe.step * 1000,
        timeout             = Probe#probe.timeout * 1000,
        parents             = Probe#probe.parents,
        childs_waiting      = [],
        loggers_state       = [],
        inspectors_state    = []
    },
    {ok, S2}    = init_loggers(S1),
    {ok, S3}    = init_inspectors(S2),
    {ok, SF}    = init_probe(S3),
    RandomStart = tracker_misc:random(SF#ps_state.step),
    {ok, 'SLEEPING', SF, RandomStart}.
        
%%
%% 'SLEEPING' and 'WAITING-REPLY'  state, in normal running operations
%%
%% ALL STATES
'SLEEPING'(timeout, SData) ->
    ?LOG("timeout triggered sleeping"),
    % launch probe will send_event({reply, Val})
    erlang:spawn(?MODULE, launch_probe, [SData]),
    {next_state, 'WAITING-REPLY', SData}.

'WAITING-REPLY'({probe_reply, NewSData, PReturn}, SData) ->
    % do something with reply then trigger a late timeout
    %?LOG("handle reply"),
    %?LOG(erlang:process_info(self())),
    handle_probe_reply(SData, NewSData, PReturn),
    {next_state, 'SLEEPING', NewSData, NewSData#ps_state.step}.

%% CHILD STATES
'NEGOCIATE-WAITING-REPLY'({probe_reply, NewSData, PReturn}, SData) ->
    % handle probe reply normaly, but stop check and wait for parent reply
    handle_probe_reply(SData, NewSData, PReturn),
    {next_state, 'SLEEPING', NewSData, NewSData#ps_state.step}.

'NEGOCIATE-SLEEPING'({parent_reply, _}, SData) ->
    % handle parent replies then continue check.
    % if all parent have responded go 'SLEEPING' else stay 'NEGOCIATE-SLEEPING'
    {next_state, 'SLEEPING', SData, SData#ps_state.step}.


handle_event({if_parent_is_ok, Status}, _SName, SData) ->
    io:format("if parent_is_ok, ~p~n", [Status]),
    #ps_state{parents   = Parents}  = SData,
    #ps_state{probe     = Probe}    = SData,
    #probe{name         = Name}     = Probe,
    send_child_query(Name, Parents),
    % triggered just before probe_reply comme. Query the parents here.
    {next_state, 'NEGOCIATE-WAITING-REPLY', SData};

handle_event({child_query, Child}, SName, SData) ->
    io:format("child query, ~p~n", [Child]),
    #ps_state{childs_waiting    = Childs}   = SData,
    #ps_state{parents           = Parents}  = SData,
    #ps_state{probe             = Probe}    = SData,
    #probe{name                 = Name}     = Probe,
    send_child_query(Name, Parents),
    NChilds = lists:append([Child], [Childs]),
    NSData  = SData#ps_state{childs_waiting = NChilds},
    case SName of
        'WAITING-REPLY' -> 
            {next_state, 'NEGOCIATE-WAITING-REPLY', NSData};
        'SLEEPING' ->
            {next_state, 'NEGOCIATE-SLEEPING', NSData};
        _ ->
            {next_state, SName, NSData}
    end;

handle_event({sync_request, CState}, SName, SData) ->
    #ps_state{probe     = Probe}    = SData,
    #probe{name         = Name}     = Probe,
    Pdus    = log_dump(SData),
    ok      = send_unicast(CState, Pdus),
    ok      = gen_channel:subscribe(Name, CState),
    {next_state, SName, SData}.

handle_sync_event(get_perms, _From, SName, SData) ->
    #ps_state{probe     = Probe}        = SData,
    #probe{permissions  = Permissions}  = Probe,
    {reply, Permissions, SName, SData}.

handle_info(_Info, SName, SData) ->
    {next_state, SName, SData}.

terminate(_Reason, _SName, _SData) ->
    normal.

code_change(_OldVsn, SName, SData, _Extra) ->
    {ok, SName, SData}.


%%
%% UTILS
%%
-spec launch_probe(#ps_state{}) -> ok.
% @doc
% It is the spawned proc who call the "gen_probe" module defined in the 
% #probe{} record. Called from "initial_pass" and "next_pass" modules.
% @end
launch_probe(#ps_state{probe  = Probe } = S) ->
    Mod         = Probe#probe.tracker_probe_mod,
    ProbeReturn = Mod:exec({S, Probe}),
    NewSData    = inspect(S, ProbeReturn),
    gen_fsm:send_event(Probe#probe.pid, {probe_reply, NewSData, ProbeReturn}).

  
% INIT PROBE
% TODO keystore conf here
-spec init_probe(#ps_state{}) -> #ps_state{}.
init_probe(#ps_state{
        probe = #probe{tracker_probe_mod = Mod}
    } = S) ->
    SF = Mod:init(S),
    {ok, SF}.

% LOGGERS
% TODO keystore conf here
-spec init_loggers(#ps_state{}) -> #ps_state{}.
init_loggers(#ps_state{probe = Probe} = State) ->
    NewState = lists:foldl(
        fun(#logger{module = Mod, conf = Conf}, PSState) ->
            {ok, NPSState} = Mod:init(Conf, PSState),
            NPSState
        end, State, Probe#probe.loggers),
    {ok, NewState}.

% TODO send conf here
-spec log(#ps_state{}, {atom(), any()}) -> ok.
log(#ps_state{probe = Probe} = PSState, Msg) ->
    lists:foreach(fun(#logger{module = Mod}) ->
        spawn(fun() -> Mod:log(PSState, Msg) end)
    end, Probe#probe.loggers),
    ok.

-spec log_dump(#ps_state{}) -> any().
log_dump(#ps_state{probe = Probe} = PSState) ->
    L = [Mod:dump(PSState) || 
            #logger{module = Mod} <- Probe#probe.loggers],
    L2 = lists:filter(fun(Element) ->
        case Element of
            ignore  -> false;
            _       -> true
        end
    end, L),
    L2.

% INSPECTORS
% TODO keystore conf here
-spec init_inspectors(#ps_state{}) -> #ps_state{}.
init_inspectors(#ps_state{probe = Probe} = State) ->
    Inspectors = Probe#probe.inspectors,
    NewState = lists:foldl(fun(#inspector{module = Mod, conf = Conf}, S) ->
        {ok, NewState} = Mod:init(Conf, S),
        NewState 
    end, State, Inspectors),
    {ok, NewState}.

% TODO send conf here
-spec inspect(#ps_state{}, Msg::tuple()) -> #ps_state{}.
inspect(#ps_state{probe = Probe} = State, Msg) ->
    Inspectors = Probe#probe.inspectors,
    {State, NewState, Msg} = lists:foldl(
        fun(#inspector{module = Mod}, {Orig, Modified, Message}) ->
            {ok, New} = Mod:inspect(Orig, Modified, Message),
            {Orig, New, Message}
        end, {State, State, Msg}, Inspectors),
    NewState.


%%
%% HANDLE PROBE REPLY
%%
handle_probe_reply(SData, NSData, PR) ->
    #ps_state{probe = ProbeS1}  = SData,
    #ps_state{probe = ProbeS2}  = NSData,

    case ProbeS1 of
        ProbeS2 ->
            emit_local(SData, PR);
        _ ->
            emit_local(NSData, PR),
            emit_wide(NSData, PR)
    end.

% Status is identical notify all the subscribers of the probe
emit_local(SData, PR) ->
    #ps_state{probe     = Probe}    = SData,
    #ps_state{target    = Target}   = SData,
    #probe{name         = Name}     = Probe,
    #probe{permissions  = Perms}    = Probe,
    #target{id          = TargetId} = Target,
    Pdu = tracker_pdu:probe_return({PR, TargetId, Name}),
    supercast_mpd:multicast_msg(Name, {Perms, Pdu}),
    log(SData, PR).

% Status is différent notify all the subscribers of the master channel
emit_wide(NSData, PR) ->
    #ps_state{probe     = Probe}    = NSData,
    #ps_state{target    = Target}   = NSData,
    #target{id          = TargetId} = Target,
    #probe{id           = ProbeId}  = Probe,
    tracker_target_channel:update(TargetId, ProbeId, {Probe, PR}).
%%
%%
%%

% send to a #client_state{}
send_unicast(CState, Pdus) ->
    #client_state{module = Mod} = CState,
    lists:foreach(fun(Pdu) -> Mod:send(Pdu) end, Pdus).

send_child_query(_, [])       -> ok;
send_child_query(Name, [H|T]) ->
    tracker_probe_fsm:child_query(H, {child_query, Name}),
    send_child_query(Name, T).

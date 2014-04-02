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

% start
-export([
    start_link/1
]).

% gen_channel
-export([
    get_perms/1,
    sync_request/2
]).

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
    'INITIALIZE'/2,
    'INITIALIZE'/3,
    'INITIALIZE-WAITING-REPLY'/2,
    'INITIALIZE-WAITING-REPLY'/3,
    'SLEEPING-STEP'/2,
    'WAITING-REPLY'/2,
    'WAITING-CRITICAL-REPLY'/2,
    'STOP-REACHABLE'/2,
    'STOP-UNREACHABLE'/2
]).

% local api
-export([
    if_parent_is_ok/2,  % from btracker_inspector_parent:inspect
    register_child/2,   % from tracker_probe_fsm:'INITIALIZE'
    freeze/1,           % from tracker_probe_sup:parents_sync/0
    synchronize_parents/1,  % from tracker_probe_sup:parents_sync/0
    launch/1,           % from tracker_probe_sup:parents_sync/0
    launch_probe/1,     % defined here for spawn(Mod,fun,arg)
    emit_parent_move/3, % when a probe change state, emit to childs.
    force_check/1
]).


% fsm behaviour
start_link({Target, #probe{name = Name} = Probe}) ->
    gen_fsm:start_link({local, Name}, ?MODULE, [Target, Probe], []).

% gen_channel behaviour
get_perms(PidName) ->
    gen_fsm:sync_send_all_state_event(PidName, get_perms).

sync_request(PidName, CState) ->
    gen_fsm:send_all_state_event(PidName, {sync_request, CState}).

% FSM funs
% from inspector_parent
-spec if_parent_is_ok(atom(), atom()) -> ok.
% @doc
% Triggered by btracker_inspector_parent:inspect/2 that the probe must
% check his parents to determinate the real status of the probe (
% 'CRITICAL' if parent is 'OK' or 'UNKNOWN' if the parent is 'CRITICAL'
% or 'UNKNOWN'.
% @end
if_parent_is_ok(PidName, Status) ->
    gen_fsm:send_all_state_event(PidName, {if_parent_is_ok, Status}).

% from tracker_probe_fsm:'INITIALIZE'/2
-spec freeze(pid()) -> ok.
% @doc
% Called by tracker_probe_sup before parents init. This function
% every actions and put the probe in 'INITIALIZE' state. If the state
% of the probe was 'WAITING-REPLY' put the probe in 
% 'INITIALIZE-WAITING-REPLY' state.
% @end
freeze(PidName) ->
    gen_fsm:sync_send_all_state_event(PidName, freeze).

-spec synchronize_parents(pid()) -> ok.
% @doc
% The probe MUST be in 'INITIALIZE' state to execute this function.
% Called by tracker_probe_sup after freeze/1.
% @end
synchronize_parents(PidName) ->
    gen_fsm:sync_send_event(PidName, register_to_parents).

-spec launch(pid()) -> ok.
% @doc
% Start the probe. The probe MUST be in 'INITIALIZE' state to execute
% this function. Put the probe in 'SLEEPING-STEP' state with a random
% start from 0 to #probe.step.
% @end
launch(PidName) ->
    gen_fsm:send_event(PidName, launch).

-spec emit_parent_move(atom(), atom(), atom()) -> ok.
% @doc
% When a probe move occur, the probe execute this function to all childs
% he knows about.
% @end
emit_parent_move(PidName, Parent, Status) ->
    gen_fsm:send_all_state_event(PidName, {parent_move, Parent, Status}).

force_check(PidName) ->
    gen_fsm:send_all_state_event(PidName, force_check).

register_child(PidName, Child) ->
    gen_fsm:send_event(PidName, {register_child, Child}).

init([Target, Probe]) ->
    #probe{parents = Parents} = Probe,
    ProbeParents = [{Parent, undefined} || Parent <- Parents],
    S1 = #ps_state{
        target              = Target,
        probe               = Probe#probe{pid = self()},
        step                = Probe#probe.step * 1000,
        timeout             = Probe#probe.timeout * 1000,
        fsm_parents         = ProbeParents,
        fsm_childs          = [],
        loggers_state       = [],
        inspectors_state    = []
    },
    {ok, S2}    = init_loggers(S1),
    {ok, S3}    = init_inspectors(S2),
    {ok, SF}    = init_probe(S3),
    {ok, 'INITIALIZE', SF}.
        
%%
%% 'SLEEPING' and 'WAITING-REPLY'  state, in normal running operations
%%
%% ALL STATES
'INITIALIZE'({register_child, Child}, SData) ->
    ?LOG({'register child', Child}),
    #ps_state{fsm_childs = Childs} = SData,
    NewChilds   = [Child | Childs],
    NewSData    = SData#ps_state{fsm_childs = NewChilds},
    {next_state, 'INITIALIZE', NewSData};

'INITIALIZE'(launch, SData) ->
    ?LOG({'launch'}),
    RandomStep = tracker_misc:random(SData#ps_state.step),
    {next_state, 'SLEEPING-STEP', SData, RandomStep};

'INITIALIZE'(Any, SData) ->
    ?LOG({"INITIALIZE guard receive", Any}),
    {next_state, 'INITIALIZE', SData}.

'INITIALIZE'(register_to_parents, _From, SData) ->
    ?LOG({'register to parent'}),
    #ps_state{fsm_parents   = Parents}  = SData,
    #ps_state{probe         = Probe}    = SData,
    #probe{name             = Name}     = Probe,
    register_to_parents(Name, Parents),
    {reply, ok, 'INITIALIZE', SData}.




'INITIALIZE-WAITING-REPLY'({register_child, Child}, SData) ->
    #ps_state{fsm_childs = Childs} = SData,
    NewChilds   = [Child | Childs],
    NewSData    = SData#ps_state{fsm_childs = NewChilds},
    {next_state, 'INITIALIZE-WAITING-REPLY', NewSData};

'INITIALIZE-WAITING-REPLY'({probe_reply, _NewSData, _PReturn}, SData) ->
    {next_state, 'INITIALIZE', SData};

'INITIALIZE-WAITING-REPLY'(launch, SData) ->
    RandomStep = tracker_misc:random(SData#ps_state.step),
    {next_state, 'WAITING-REPLY', SData, RandomStep};

'INITIALIZE-WAITING-REPLY'(Any, SData) ->
    ?LOG({"INITIALIZE-WAITING-REPLY guard receive", Any}),
    {next_state, 'INITIALIZE', SData}.

'INITIALIZE-WAITING-REPLY'(register_to_parents, _From, SData) ->
    #ps_state{fsm_parents   = Parents}  = SData,
    #ps_state{probe         = Probe}    = SData,
    #probe{name             = Name}     = Probe,
    register_to_parents(Name, Parents),
    {reply, ok, 'INITIALIZE-WAITING-REPLY', SData}.

register_to_parents(_, []) -> ok;
register_to_parents(Child, [Parent|Tail]) ->
    tracker_probe_fsm:register_child(Parent, Child),
    register_to_parents(Child, Tail).




%%
'SLEEPING-STEP'(timeout, SData) ->
    ?LOG("SLEEPING"),
    erlang:spawn(?MODULE, launch_probe, [SData]),
    {next_state, 'WAITING-REPLY', SData}.

'WAITING-REPLY'({probe_reply, NewSData, PReturn}, SData) ->
    ?LOG("WAITING-REPLY"),
    handle_probe_reply(SData, NewSData, PReturn),
    {next_state, 'SLEEPING-STEP', NewSData, NewSData#ps_state.step}.



request_parent_update(SData) when is_tuple(SData) ->
    #ps_state{fsm_parents = Parents} = SData,
    request_parent_update(Parents);
request_parent_update([]) -> ok;
request_parent_update([Parent|T]) ->
    tracker_probe_fsm:force_check(Parent),
    request_parent_update(T).


'WAITING-CRITICAL-REPLY'({probe_reply, NewSData, PReturn}, SData) ->
    ?LOG('WAITING-CRITICAL-REPLY'),
    #ps_state{fsm_parents   = Parents}  = SData,
    case parents_alive(Parents) of
        true    ->
            % do something to update parent to be sure
            request_parent_update(SData),
            StopParents = [{Parent, stop_reachable} || {Parent, _} <- Parents],
            NewSData1   = SData#ps_state{fsm_stop_parents = StopParents},
            NewSData2   = NewSData1#ps_state{
                fsm_pending_crit_reply = {NewSData, PReturn}
            },
            {next_state, 'STOP-REACHABLE', NewSData2};
        false   ->
            % i am not recheable. keep the 'UNKNOWN' probe status set by the
            % btracker_inspector_parent module and 'STOP'.
            handle_probe_reply(SData, NewSData, PReturn),
            % Initialize the fsm_stop_parents to nothing and let 'STOP'
            % update it to reach a state which permit to continue.
            {next_state, 'STOP-UNREACHABLE', NewSData}
    end.

'STOP-UNREACHABLE'({parent_move, _, _}, SData) ->
    #ps_state{fsm_parents       = Parents}      = SData,
    case parents_alive(Parents) of
        true    ->
            {next_state, 'SLEEPING',  SData, 100};
        false   ->
            {next_state, 'STOP-UNREACHABLE',      SData}
    end.

'STOP-REACHABLE'({parent_move, Parent, Status}, SData) ->
    #ps_state{fsm_stop_parents  = Parents}      = SData,
    NewParents  = lists:keystore(Parent, 1, Parents, {Parent, Status}),
    NewSData    = SData#ps_state{fsm_stop_parents = NewParents},
    case check_complete(NewParents) of
        true ->
            case parents_alive(NewParents) of
                true ->
                    % go critical
                    #ps_state{fsm_pending_crit_reply = Reply} = SData,
                    {CritSData, PReturn} = Reply,
                    #ps_state{probe = CritProbe} = CritSData,
                    NewCritProbe = CritProbe#probe{status = 'CRITICAL'},
                    NewCritSData = CritSData#ps_state{probe = NewCritProbe},
                    handle_probe_reply(SData, NewCritSData, PReturn),
                    Step = CritSData#ps_state.step,
                    {next_state, 'SLEEPING-STEP', CritSData, Step};
                false ->
                    #ps_state{fsm_pending_crit_reply = Reply} = SData,
                    {CritSData, PReturn} = Reply,
                    handle_probe_reply(SData, CritSData, PReturn),
                    {next_state, 'STOP-UNREACHABLE', CritSData}
            end;
        false ->
            {next_state, 'STOP-REACHABLE', NewSData}
    end.

check_complete([]) -> true;
check_complete([{_, stop_reachable} | _]) -> false;
check_complete([_|T]) -> check_complete(T).

parents_alive([]) -> false;
parents_alive([{_,Status}|H]) ->
    case Status of
        'CRITICAL'      -> parents_alive(H);
        'UNKNOWN'       -> parents_alive(H);
        stop_reachable  -> parents_alive(H);
        _               -> true
    end.
    


handle_event({if_parent_is_ok, Status}, 'WAITING-REPLY', SData) ->
    io:format("if parent_is_ok, ~p~n", [Status]),
    #ps_state{fsm_parents   = Parents}  = SData,
    #ps_state{probe         = Probe}    = SData,
    #probe{name             = Name}     = Probe,
    ?LOG({Parents, Name}),
    {next_state, 'WAITING-CRITICAL-REPLY', SData};

handle_event({parent_move, Parent, Status}, SName, SData) ->
    #ps_state{fsm_parents   = Parents}  = SData,
    NewParents  = lists:keystore(Parent, 1, Parents, {Parent, Status}),
    NewSData    = SData#ps_state{fsm_parents = NewParents},
    case SName of
        'STOP-UNREACHABLE' ->
            gen_fsm:send_event(self(), parent_move),
            {next_state, SName, NewSData};
        'STOP-REACHABLE' ->
            gen_fsm:send_event(self(), parent_move),
            {next_state, SName, NewSData};
        _ ->
            {next_state, SName, NewSData}
    end;

handle_event(force_check, SName, SData) ->
    #ps_state{fsm_childs = Childs}  = SData,
    #ps_state{probe      = Probe}   = SData,
    #probe{name          = Name}    = Probe,
    #probe{status        = Status}  = Probe,
    case SName of
        'STOP-UNREACHABLE' ->
            emit_status(Status, Name, Childs),
            {next_state, SName, SData};
        'STOP-REACHABLE' ->
            emit_status(Status, Name, Childs),
            {next_state, SName, SData};
        'SLEEPING-STEP' ->
            {next_state, SName, SData, 500};
        _ ->
            {next_state, SName, SData}
    end;


% GEN_CHANNEL event
handle_event({sync_request, CState}, SName, SData) ->
    #ps_state{probe     = Probe}    = SData,
    #probe{name         = Name}     = Probe,
    Pdus    = log_dump(SData),
    ok      = send_unicast(CState, Pdus),
    ok      = gen_channel:subscribe(Name, CState),
    {next_state, SName, SData}.

emit_status(_,         _, []) -> ok;
emit_status(Status, Name, [Child|T]) ->
    tracker_probe_fsm:emit_parent_move(Child, Name, Status),
    emit_status(Status, Name, T).

% GEN_CHANNEL event
handle_sync_event(get_perms, _From, SName, SData) ->
    #ps_state{probe     = Probe}        = SData,
    #probe{permissions  = Permissions}  = Probe,
    {reply, Permissions, SName, SData};

% tracker_probe_sup event
handle_sync_event(freeze, _From, SName, SData) ->
    case SName of
        'WAITING-REPLY' ->
            {reply, ok, 'INITIALIZE-WAITING-REPLY', SData};
        _ ->
            {reply, ok, 'INITIALIZE', SData}
    end.

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
            emit_childs(NSData),
            emit_local(SData, PR);
        _ ->
            emit_childs(NSData),
            emit_local(NSData, PR),
            emit_wide(NSData, PR)
    end.

% Notify all the subscribers of the probe (probeReturn)
emit_local(SData, PR) ->
    #ps_state{probe     = Probe}    = SData,
    #ps_state{target    = Target}   = SData,
    #probe{name         = Name}     = Probe,
    #probe{permissions  = Perms}    = Probe,
    #target{id          = TargetId} = Target,
    Pdu = tracker_pdu:probe_return({PR, TargetId, Name}),
    gen_channel:emit(Name, {Perms, Pdu}),
    log(SData, PR).

% Notify all the subscribers of the master channel (probeInfo)
emit_wide(NSData, PR) ->
    #ps_state{probe     = Probe}    = NSData,
    #ps_state{target    = Target}   = NSData,
    #target{id          = TargetId} = Target,
    #probe{id           = ProbeId}  = Probe,
    tracker_target_channel:update(TargetId, ProbeId, {Probe, PR}).

% Notify all childs of the status event
emit_childs(NSData) ->
    #ps_state{probe      = Probe}   = NSData,
    #ps_state{fsm_childs = Childs}  = NSData,
    #probe{status        = Status}  = Probe,
    #probe{name          = Name}    = Probe,
    emit_childs(Status, Name, Childs).

emit_childs(_, _, []) -> ok;
emit_childs(Status, Name, [Child|Tail]) ->
    tracker_probe_fsm:emit_parent_move(Child, Name, Status),
    emit_childs(Status, Name, Tail).

%%
%%
%%

% send to a #client_state{}
send_unicast(CState, Pdus) ->
    #client_state{module = Mod} = CState,
    lists:foreach(fun(Pdu) -> Mod:send(Pdu) end, Pdus).

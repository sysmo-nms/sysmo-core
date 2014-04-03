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

    'OK'/2,
    'WARNING'/2,
    'CRITICAL'/2,
    'UNKNOWN'/2,

    'STOP-REACHABLE'/2,
    'STOP-UNREACHABLE'/2
]).

% local api
-export([
    critical_return/1,  % from btracker_inspector_parent:inspect
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
    %?LOG({api_call, Name, start_link}),
    gen_fsm:start_link({local, Name}, ?MODULE, [Target, Probe], []).

% gen_channel behaviour
get_perms(PidName) ->
    gen_fsm:sync_send_all_state_event(PidName, get_perms).

sync_request(PidName, CState) ->
    gen_fsm:send_all_state_event(PidName, {sync_request, CState}).

% FSM funs
% from inspector_parent
-spec critical_return(atom()) -> ok.
% @doc
% Triggered by btracker_inspector_parent:inspect/2 that the probe must
% check his parents to determinate the real status of the probe (
% 'CRITICAL' if parent is 'OK' or 'UNKNOWN' if the parent is 'CRITICAL'
% or 'UNKNOWN'.
% @end
critical_return(PidName) ->
    %?LOG({api_call, PidName, critical_return}),
    gen_fsm:send_all_state_event(PidName, critical_return).

% from tracker_probe_fsm:'INITIALIZE'/2
-spec freeze(pid()) -> ok.
% @doc
% Called by tracker_probe_sup before parents init. This function
% every actions and put the probe in 'INITIALIZE' state. If the state
% of the probe was 'WAITING-REPLY' put the probe in 
% 'INITIALIZE-WAITING-REPLY' state.
% @end
freeze(PidName) ->
    %?LOG({api_call, freeze, PidName}),
    gen_fsm:sync_send_all_state_event(PidName, freeze).

-spec synchronize_parents(pid()) -> ok.
% @doc
% The probe MUST be in 'INITIALIZE' state to execute this function.
% Called by tracker_probe_sup after freeze/1.
% @end
synchronize_parents(PidName) ->
    %?LOG({api_call, PidName, synchronize_parents}),
    gen_fsm:sync_send_event(PidName, register_to_parents).

-spec launch(pid()) -> ok.
% @doc
% Start the probe. The probe MUST be in 'INITIALIZE' state to execute
% this function. Put the probe in 'SLEEPING-STEP' state with a random
% start from 0 to #probe.step.
% @end
launch(PidName) ->
    %?LOG({api_call, PidName, launch}),
    gen_fsm:send_event(PidName, launch).

-spec emit_parent_move(atom(), any(), atom(), atom()) -> ok.
% @doc
% When a probe move occur, the probe execute this function to all childs
% he knows about.
% @end
emit_parent_move(PidName, Ts, Parent, Status) ->
    %?LOG({api_call, PidName, emit_parent_move, Parent, Status}),
    gen_fsm:send_all_state_event(PidName, {parent_move, Parent, Status, Ts}).

force_check(PidName) ->
    %?LOG({api_call, PidName, force_check}),
    gen_fsm:send_all_state_event(PidName, force_check).

register_child(PidName, Child) ->
    %?LOG({api_call, PidName, register_child, Child}),
    gen_fsm:send_event(PidName, {register_child, Child}).

init([Target, Probe]) ->
    %?LOG({gen_fsm_init, Probe#probe.name, init}),
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
register_to_parents(_, []) -> ok;
register_to_parents(Child, [{Parent, _}|Tail]) ->
    tracker_probe_fsm:register_child(Parent, Child),
    register_to_parents(Child, Tail).

'INITIALIZE'(register_to_parents, _From, SData) ->
    #ps_state{fsm_parents   = Parents}  = SData,
    #ps_state{probe         = Probe}    = SData,
    #probe{name             = Name}     = Probe,
    register_to_parents(Name, Parents),
    {reply, ok, 'INITIALIZE', SData}.

'INITIALIZE'({register_child, Child}, SData) ->
    #ps_state{fsm_childs = Childs} = SData,
    NewChilds   = [Child | Childs],
    NewSData    = SData#ps_state{fsm_childs = NewChilds},
    %?LOG({childs_are, NewChilds}),
    {next_state, 'INITIALIZE', NewSData};

'INITIALIZE'(launch, SData) ->
    #ps_state{probe = Probe}    = SData,
    #probe{status   = Status}   = SData,
    RandomStep  = tracker_misc:random(SData#ps_state.step),
    TRef        = launch_probe(SData, RandomStep),
    NewSData    = SData#ps_state{tref = TRef}
    NewSData2   = NewSData#ps_state{check_state = sleeping},
    {next_state, Status, SData};



%%
'OK'(parent_move, SData) ->
    {next_state, 'OK', SData}.

'WARNING'(parent_move, SData) ->
    {next_state, 'WARNING', SData}.

'CRITICAL'(parent_move, SData) ->
    {next_state, 'CRITICAL', SData}.

'UNKNOWN'(parent_move, SData) ->
    {next_state, 'UNKNOWN', SData}.

'NEGOCIATE'({parent_move, Parent, Ts, Status}, SData) ->
    #ps_state{nego_parents  = NegoParents}  = SData,
    #ps_state{nego_return  = NegoReturn}    = SData,
    #ps_state{last_return  = Last}          = SData,
    case timer:now_diff(Ts, Last) < 0 of
        true ->
            NegoParents2 = lists:keystore(Parent, 1, NegoParents, {Parent, Status});
        false ->
            NegoParents2 = NegoParents
    end,

    case nego_complete(NegoParents2) of
        true  ->
            case parents_alive(NegoParents2) of
                true    ->
                    {_, NewSData, PR} = NegoReturn,
                    handle_probe_reply(SData, NewSData, PReturn),
                    launch_probe(NewSData, Step),
                    {next_state, 'CRITICAL', NewSData}
                false   ->
                    {_, NewSData, PR}           = NegoReturn,
                    #ps_state{probe = NewProbe} = NewSData,
                    NewProbe2 = NewProbe#probe{status = 'UNKNOWN'}, % TODO change status to HALT
                    NewSData2 = NewSData#ps_state{probe = NewProbe2},
                    handle_probe_reply(SData, NewSData2, PReturn),
                    {next_state, 'HALT', NewSData2}
                end;

        false ->
            NewSData = SData#ps_state{nego_parents = NegoParents2},
            {next_state, 'NEGOCIATE', NewSData}
    end.
        
% wait for parents to come up
'HALT'(parent_move, SData) ->
    #ps_state{parents   = Parents}  = SData,
    #ps_state{step      = Step}     = SData,

    case parents_alive(Parents) of
        true ->
            launch_probe(SData, Step),
            {next_state, 'TAKE-OVER', SData};
        false ->
            {next_state, 'HALT', SData}
    end.

% in 'TAKE-OVER' state, return in HALT state if parent come down again.
'TAKE-OVER'(parent_move, SData) ->
    #ps_state{parents   = Parents}  = SData,
    case parents_alive(Parents) of
        true ->
            {next_state, 'TAKE-OVER', SData};
        false ->
            {next_state, 'HALT', SData}
    end.
            

handle_event({probe_return, NewSData, PR}, SName, SData) ->
    #ps_state{force_check = Force}     = SData
    #ps_state{parents   = Parents}     = SData,
    #ps_state{probe     = OldProbe}    = SData,
    #ps_state{probe     = NewProbe}    = NewSData,
    #probe{status       = OldStatus}   = OldProbe,
    #probe{status       = NewStatus}   = NewProbe,

    #ps_state{step      = NormalStep}  = SData,
    case Force of
        true ->
            Step = 1;
        false ->
            Step = NormalStep
    end;

    case NewStatus of
        'OK' ->
            handle_probe_reply(SData, NewSData, PReturn),
            TRef        = launch_probe(NewSData, Step),
            NewSData1   = NewSData#ps_state{tref = TRef}
            NewSData2   = NewSData1#ps_state{check_state = sleeping},
            {next_state, 'OK', NewSData};
        'WARNING' ->
            handle_probe_reply(SData, NewSData, PReturn),
            TRef        = launch_probe(NewSData, Step),
            NewSData1   = NewSData#ps_state{tref = TRef}
            NewSData2   = NewSData1#ps_state{check_state = sleeping},
            {next_state, 'WARNING', NewSData};
        'UNKNOWN' ->
            handle_probe_reply(SData, NewSData, PReturn),
            TRef        = launch_probe(NewSData, Step),
            NewSData1   = NewSData#ps_state{tref = TRef}
            NewSData2   = NewSData1#ps_state{check_state = sleeping},
            {next_state, 'UNKNOWN', NewSData};
        'CRITICAL' ->
            case OldStatus of
                'CRITICAL' -> % NEGOCIATE allready done
                    case parents_alive(Parents) of
                        true ->
                            handle_probe_reply(SData, NewSData, PReturn),
                            launch_probe(NewSData, Step),
                            TRef        = launch_probe(NewSData, Step),
                            NewSData1   = NewSData#ps_state{tref = TRef}
                            NewSData2   = NewSData1#ps_state{check_state = sleeping},
                            {next_state, 'CRITICAL', NewSData2};
                        false ->
                            NewProbe2 = NewProbe#probe{status = 'UNKNOWN'}, % TODO change status to HALT
                            NewSData2 = NewSData#ps_state{probe = NewProbe2},
                            handle_probe_reply(SData, NewSData2, PReturn),
                            {next_state, 'HALT', NewSData2}
                    end;
                _ ->
                    NegoReturn = {probe_return, NewSData, PR},
                    NegoList   = [{Parent, unknown} || {Parent, _} <- Parents],
                    NewSData2  = NewSData#ps_state{nego_return   = NegoReturn},
                    NewSData3  = NewSData2#ps_state{nego_parents = NegoList},
                    request_parent_update(NewSData3),
                    {next_state, 'NEGOCIATE', NewSData3}
            end
    end;

handle_event(force_check, SName, SData) ->
    #ps_state{tref = TRef}           = SData,
    case State of
        sleeping ->
            timer:cancel(TRef),
            TRef        = launch_probe(SData, 1),
            NewSData    = SData#ps_state{tref = TRef}
            NewSData2   = NewSData#ps_state{check_state = sleeping},
            {next_state, SName, NewSData2};
        running  ->
            {next_state, SName, SData#ps_state{force_check = true}
    end.



% GEN_CHANNEL event

% prevent 2 probes to be launched in paralel. Possible in a race condition
% with force_check. Stop the late check is sufficient.
handle_sync_event(check_start, _From, SName, SData) ->
    #ps_state{probe_state = State} = SData,
    case State of
        sleeping ->
            {reply, true, SName, SData#ps_state{probe_state = running}}.
        running  ->
            {reply, false, SName, SData}
    end;

% tracker_probe_sup event
handle_sync_event(freeze, _From, SName, SData) ->
    %?LOG({gen_fsm_sync_all_events, SData#ps_state.probe#probe.name, {freeze}, {current, SName}}),
    case SName of
        'WAITING-REPLY' ->
            {reply, ok, 'INITIALIZE-WAITING-REPLY', SData};
        _ ->
            {reply, ok, 'INITIALIZE', SData}
    end;

    
handle_sync_event(get_perms, _From, SName, SData) ->
    #ps_state{probe     = Probe}        = SData,
    #probe{permissions  = Permissions}  = Probe,
    {reply, Permissions, SName, SData}.







% GEN_CHANNEL event
handle_event({sync_request, CState}, SName, SData) ->
    #ps_state{probe     = Probe}    = SData,
    #probe{name         = Name}     = Probe,
    Pdus    = log_dump(SData),
    ok      = send_unicast(CState, Pdus),
    ok      = gen_channel:subscribe(Name, CState),
    {next_state, SName, SData}.

request_parent_update(SData) when is_tuple(SData) ->
    #ps_state{fsm_parents = Parents} = SData,
    %?LOG({request_parent_update, Parents}),
    request_parent_update(Parents);
request_parent_update([]) -> ok;
request_parent_update([Parent|T]) ->
    tracker_probe_fsm:force_check(Parent),
    request_parent_update(T).

nego_complete([]) -> true;
nego_complete([{_, stop_reachable} | _]) -> false;
nego_complete([_|T]) -> nego_complete(T).

parents_alive([]) -> false;
parents_alive([{_,Status}|H]) ->
    case Status of
        'CRITICAL'      -> parents_alive(H);
        'UNKNOWN'       -> parents_alive(H);
        check_reachable -> parents_alive(H);
        _               -> true
    end.

emit_status(_,         _, []) -> ok;
emit_status(Status, Name, [Child|T]) ->
    tracker_probe_fsm:emit_parent_move(Child, Name, Status),
    emit_status(Status, Name, T).


handle_info(_Info, SName, SData) ->
    {next_state, SName, SData}.

terminate(_Reason, _SName, _SData) ->
    normal.

code_change(_OldVsn, SName, SData, _Extra) ->
    {ok, SName, SData}.


%%
%% UTILS
%%
-spec launch_probe(#ps_state{}, int()) -> ok.
% @doc
% It is the spawned proc who call the "gen_probe" module defined in the 
% #probe{} record. Called from "initial_pass" and "next_pass" modules.
% @end
launch_probe(SData, Step) ->
    #ps_state{probe = Probe}        = SData,
    #probe{tracker_probe_mod = Mod} = Probe,
    {ok, TRef} = timer:apply_after(Step, ?MODULE, launch_probe, [Mod, SData, Probe]),
    TRef.
launch_probe(Mod, S, Probe) ->
    Pid = Probe#probe.pid,
    Ret = gen_fsm:sync_send_all_state_event(Pid, check_start).
    case Ret of
        true ->
            ProbeReturn = Mod:exec({S, Probe}),
            NewSData    = inspect(S, ProbeReturn),
            NewSData2   = NewSData#ps_state{tref = undefined},
            NewSData3   = NewsData2#ps_state{last_check = erlang:now()},
            gen_fsm:send_all_state_event(Pid, {probe_reply, NewSData3, ProbeReturn}).
        false ->
            ok
    end.

  
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
    #ps_state{last_check = Ts}      = NSData,
    #probe{status        = Status}  = Probe,
    #probe{name          = Name}    = Probe,
    %?LOG({api_call, Name, emit_childs, Status, Childs}),
    emit_childs(Status, Ts, Name, Childs).

emit_childs(_, _, _, []) -> ok;
emit_childs(Status, Ts, Name, [Child|Tail]) ->
    tracker_probe_fsm:emit_parent_move(Child, Ts, Name, Status),
    emit_childs(Status, Ts, Name, Tail).

%%
%%
%%

% send to a #client_state{}
send_unicast(CState, Pdus) ->
    #client_state{module = Mod} = CState,
    lists:foreach(fun(Pdu) -> Mod:send(Pdu) end, Pdus).

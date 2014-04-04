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
%-behaviour(gen_channel).
-include("../include/tracker.hrl").

% start
-export([
    start_link/1
]).

% gen_channel
% -export([
%     get_perms/1,
%     sync_request/2
% ]).

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
    'RUNNING-FREEZED'/2,
    'RUNNING-FREEZED'/3,
    'RUNNING'/2,
    'RUNNING'/3
    %'FORCE'/2,
    %'CRITICAL'/2,
    %'NEGOCIATE'/2,
    %'HALT'/2,
    %'TAKE-OVER'/2
]).

% local api
-export([
    freeze/1,           % from tracker_probe_sup:parents_sync/0
    reset_family/1,
    synchronize_parents/1,  % from tracker_probe_sup:parents_sync/0
    synchronize_childs/1,
    launch/1,            % from tracker_probe_sup:parents_sync/0
    probe_take_of/3      % defined here for spawn(Mod,fun,arg)
    %emit_parent_move/3, % when a probe change state, emit to childs.
    %force_check/1
]).

-record(parent, {
    name,
    status,
    last_check_start
}).

-define(OK,         'OK').
-define(CRITICAL,   'CRITICAL').
-define(WARNING,    'WARNING').
-define(UNKNOWN,    'UNKNOWN').
-define(HALT,       'UNKNOWN').

% fsm behaviour
start_link({Target, #probe{name = Name} = Probe}) ->
    %?LOG({api_call, Name, start_link}),
    gen_fsm:start_link({local, Name}, ?MODULE, [Target, Probe], []).

% gen_channel behaviour
% get_perms(PidName) ->
%     gen_fsm:sync_send_all_state_event(PidName, get_perms).
% 
% sync_request(PidName, CState) ->
%     gen_fsm:send_all_state_event(PidName, {sync_request, CState}).





% FSM funs
%%
%% from tracker_probe_sup
%%
-spec freeze(pid()) -> ok.
% @doc
% Called by tracker_probe_sup before parents init. This function
% every actions and put the probe in 'INITIALIZE' state. If the state
% of the probe was 'WAITING-REPLY' put the probe in 
% 'INITIALIZE-WAITING-REPLY' state.
% @end
freeze(PidName) ->
    %?LOG({api_call, freeze, PidName}),
    gen_fsm:sync_send_event(PidName, freeze).


-spec reset_family(pid()) -> ok.
reset_family(PidName) ->
    gen_fsm:sync_send_event(PidName, reset_family).

-spec synchronize_parents(pid()) -> ok.
% @doc
% The probe MUST be in 'INITIALIZE' state to execute this function.
% Called by tracker_probe_sup after freeze/1.
% @end
synchronize_parents(PidName) ->
    %?LOG({api_call, PidName, synchronize_parents}),
    gen_fsm:sync_send_event(PidName, synchronize_parents).

synchronize_childs(PidName) ->
    gen_fsm:sync_send_event(PidName, synchronize_childs).

-spec launch(pid()) -> ok.
% @doc
% Start the probe. The probe MUST be in 'INITIALIZE' state to execute
% this function. Put the probe in 'SLEEPING-STEP' state with a random
% start from 0 to #probe.step.
% @end
launch(PidName) ->
    %?LOG({api_call, PidName, launch}),
    gen_fsm:send_event(PidName, launch).





% -spec emit_parent_move(atom(), any(), atom(), atom()) -> ok.
% % @doc
% % When a probe move occur, the probe execute this function to all childs
% % he knows about.
% % @end
% emit_parent_move(PidName, Ts, Parent, Status) ->
%     %?LOG({api_call, PidName, emit_parent_move, Parent, Status}),
%     gen_fsm:send_event(PidName, {parent_move, Parent, Status, Ts}).

% force_check(PidName) ->
%     %?LOG({api_call, PidName, force_check}),
%     gen_fsm:send_all_state_event(PidName, force_check).

init([Target, Probe]) ->
    %?LOG({gen_fsm_init, Probe#probe.name, init}),
    #probe{parents = Parents} = Probe,
    ProbeParents = [#parent{name = Parent} || Parent <- Parents],
    S1 = #ps_state{
        name                = Probe#probe.name,
        target              = Target,
        probe               = Probe#probe{pid = self()},
        step                = Probe#probe.step * 1000,
        timeout             = Probe#probe.timeout * 1000,
        parents         = ProbeParents,
        childs          = [],
        loggers_state       = [],
        inspectors_state    = []
    },
    {ok, S2}    = init_loggers(S1),
    {ok, S3}    = init_inspectors(S2),
    {ok, SF}    = init_probe(S3),
    {ok, 'RUNNING-FREEZED', SF}.
        
register_to_parents(SData) ->
    #ps_state{parents   = Parents}  = SData,
    #ps_state{name      = Name}     = SData,
    register_to_parents(Name, Parents).
register_to_parents(_, []) -> ok;
register_to_parents(Child, [{parent, Parent, _, _}|Tail]) ->
    gen_fsm:send_event(Parent, {register_child, Child}),
    register_to_parents(Child, Tail).




%%
%% RUNNING-FREEZED
%%
'RUNNING-FREEZED'(freeze, _From, SData) ->
    {reply, ok, 'RUNNING-FREEZED', SData};
'RUNNING-FREEZED'(synchronize_parents, _From, SData) ->
    register_to_parents(SData),
    {reply, ok, 'RUNNING-FREEZED', SData};
'RUNNING-FREEZED'(synchronize_childs, _From, SData) ->
    emit_childs(SData),
    {reply, ok, 'RUNNING-FREEZED', SData};
'RUNNING-FREEZED'(reset_family, _From, SData) ->
    NewSData = SData#ps_state{childs = []},
    {reply, ok, 'RUNNING-FREEZED', NewSData}.

'RUNNING-FREEZED'(launch, SData) ->
    NewSData = launch_probe(SData#ps_state{check_flag = random}),
    ?LOG({'going running', SData#ps_state.name, SData#ps_state.parents, SData#ps_state.childs}),
    {next_state, 'RUNNING', NewSData};
'RUNNING-FREEZED'({register_child, Child}, SData) ->
    #ps_state{childs    = Childs}   = SData,
    NewChilds   = [Child|Childs],
    NewSData    = SData#ps_state{childs = NewChilds},
    {next_state, 'RUNNING-FREEZED', NewSData};
'RUNNING-FREEZED'({parent_move, ParentRec}, SData) ->
    #ps_state{parents = Parents} = SData,
    #parent{name = Name}         = ParentRec,
    NewParents  = lists:keystore(Name, 2, Parents, ParentRec),
    NewSData    = SData#ps_state{parents = NewParents},
    {next_state, 'RUNNING-FREEZED', NewSData};
'RUNNING-FREEZED'({probe_reply, DiffSData, PR, _}, SData) ->
    NewSData1 = handle_probe_reply(SData, DiffSData, PR),
    {next_state, 'RUNNING-FREEZED', NewSData1}.



%%
%% RUNNING
%%
'RUNNING'(freeze, _From, SData) ->
    {reply, ok, 'RUNNING-FREEZED', SData}.

'RUNNING'({probe_reply, DiffSData, PR, _}, SData) ->
    NewSData1 = handle_probe_reply(SData, DiffSData, PR),
    NewSData2 = launch_probe(NewSData1),
    {next_state, 'RUNNING', NewSData2};

'RUNNING'({parent_move, ParentRec}, SData) ->
    ?LOG({parent_move, ParentRec}),
    #ps_state{parents = Parents} = SData,
    #parent{name = Name}         = ParentRec,
    NewParents  = lists:keystore(Name, 2, Parents, ParentRec),
    NewSData    = SData#ps_state{parents = NewParents},
    ?LOG({new_parents, NewParents}),
    {next_state, 'RUNNING', NewSData}.


handle_event(_, SName, SData) ->
    {next_state, SName, SData}.

handle_sync_event(confirm_take_of, _From, SName, SData) ->
    #ps_state{check_state = CheckState} = SData,
    case CheckState of
        ready   -> 
            NewSData = SData#ps_state{check_state = running},
            {reply, true, SName, NewSData};
        _ ->
            {reply, false, SName, SData}
    end.

launch_probe(SData) ->
    #ps_state{check_flag  = CheckFlag}  = SData,
    case CheckFlag of
        normal  -> launch_probe_normal(SData);
        force   -> launch_probe_force(SData);
        random  -> launch_probe_random(SData)
    end.
launch_probe_normal(SData) ->
    #ps_state{check_state = CheckState} = SData,
    #ps_state{step        = Step}       = SData,
    case CheckState of
        sleeping -> SData;
        running  -> SData;
        stopped  -> launch_start_sequence(SData, Step)
    end.
launch_probe_random(SData) ->
    #ps_state{check_state = CheckState} = SData,
    #ps_state{step        = TmpStep}    = SData,
    Step = tracker_misc:random(TmpStep),
    case CheckState of
        sleeping -> SData;
        running  -> SData;
        stopped  -> launch_start_sequence(SData, Step)
    end.
launch_probe_force(SData) ->
    #ps_state{check_state = CheckState} = SData,
    case CheckState of
        sleeping ->
            #ps_state{tref = TRef} = SData,
            timer:cancel(TRef),
            launch_start_sequence(SData, 0);
        running ->
            SData;
        stopped ->
            launch_start_sequence(SData, 0)
    end.



launch_start_sequence(SData, Step) ->
    #ps_state{probe          = Probe}   = SData,
    #probe{tracker_probe_mod = Mod}     = Probe,
    {ok, TRef} = timer:apply_after(
        Step, ?MODULE, probe_take_of, [Mod, SData, Probe]),
    NewSData1 = SData#ps_state{tref = TRef},
    NewSData2 = NewSData1#ps_state{check_state  = ready},
    NewSData3 = NewSData2#ps_state{check_flag   = normal},
    NewSData3.
probe_take_of(Mod, S, Probe) ->
    Pid = Probe#probe.pid,
    Ret = gen_fsm:sync_send_all_state_event(Pid, confirm_take_of),
    case Ret of
        false   -> ok;
        true    ->
            Ts          = erlang:now(),
            ProbeReturn = Mod:exec({S, Probe}),
            NewSData1   = inspect(S, ProbeReturn),
            #ps_state{probe = #probe{status = Status}}   = NewSData1,
            NewSData2   = NewSData1#ps_state{tref        = undefined},
            NewSData3   = NewSData2#ps_state{last_check  = Ts},
            NewSData4   = NewSData3#ps_state{check_state = stopped},
            gen_fsm:send_event(Pid, {probe_reply, NewSData4, ProbeReturn, Status})
    end.

handle_probe_reply(SData, NSData, PR) ->
    #ps_state{probe         = ProbeS1}      = SData,
    #ps_state{probe         = ProbeS2}      = NSData,

    % XXX keep the old check_flag because it might have changed
    % during the State modif.
    #ps_state{check_flag    = CheckFlag}    = SData,
    NSData1 = NSData#ps_state{check_flag = CheckFlag}, 


    case ProbeS1 of
        ProbeS2 ->
            emit_childs(NSData1),
            emit_local(NSData1, PR),
            NSData1;
        _ ->
            emit_childs(NSData1),
            emit_local(NSData1, PR),
            emit_wide(NSData1, PR),
            NSData1
    end.

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
emit_childs(SData) ->
    #ps_state{childs    = Childs}   = SData,
    #ps_state{name      = Name}     = SData,
    #ps_state{last_check = Ts}      = SData,
    #ps_state{probe     = Probe}    = SData,
    #probe{status       = Status}   = Probe,
    emit_childs(Status, Name, Ts, Childs).
emit_childs(_, _, _, []) -> ok;
emit_childs(Status, Name, Ts, [Child|Tail]) ->
    MoveEvent = #parent{name = Name, status = Status, last_check_start = Ts},
    gen_fsm:send_event(Child, {parent_move, MoveEvent}),
    emit_childs(Status, Ts, Name, Tail).

%launch_probe(_SData, _Step) -> ok.
%     #ps_state{probe = Probe}        = SData,
%     #probe{tracker_probe_mod = Mod} = Probe,
%     {ok, TRef} = timer:apply_after(Step, ?MODULE, launch_probe, [Mod, SData, Probe]),
%     NewSData = SData#ps_state{tref = TRef},
%     NewSData.
% launch_probe(Mod, S, Probe) ->
%     Pid = Probe#probe.pid,
%     Ret = gen_fsm:sync_send_all_state_event(Pid, check_start).
%     case Ret of
%         true ->
%             ProbeReturn = Mod:exec({S, Probe}),
%             NewSData    = inspect(S, ProbeReturn),
%             NewSData2   = NewSData#ps_state{tref = undefined},
%             NewSData3   = NewsData2#ps_state{last_check = erlang:now()},
%             gen_fsm:send_all_state_event(Pid, {probe_reply, NewSData3, ProbeReturn}).
%         false ->
%             ok
%     end.

%%
% 'RUNNING'({parent_move, _, _} = Move, SData) ->
%     #ps_state{parents = Parents} = SData,
%     NewParents = update_parents(Move, Parents),
%     {next_state, 'RUNNING', SData#ps_state{parents = NewParents}};
% 
% % 'CRITICAL', but the other status was 'CRITICAL' to. Check the parents
% % state, the probe may be unreachable now. If they are not reachable,
% % got right to the state HALT.
% 'RUNNING'({
%         probe_return, 
%         #ps_state{probe=#probe{status ='CRITICAL'}} = NewSData,
%         PR
%     },  #ps_state{probe#probe{status = 'CRITICAl'}} = SData) ->
%     #ps_state{parents = Parents} = SData,
%     #ps_state{step    = Step}    = SData,
% 
%     case parents_alive(Parents) of
%         true ->
%             handle_probe_reply(SData, NewSData, PR),
%             TRef        = launch_probe(NewSData, Step),
%             NewSData1   = NewSData#ps_state{tref = TRef}
%             NewSData2   = NewSData1#ps_state{check_state = sleeping},
%             NewSData3   = NewSData2#ps_state{force_check = false},
%             NewSData4   = NewSData3#ps_state{parents = Parents},
%             {next_state, 'RUNNING', NewSData4};
%         false ->
%             {next_state, 'HALT', NewSData4}
%     end;
% 
% % 'CRITICAL' and the latest status was not 'CRITICAL' go to 'NEGOCIATE'
% % if parents look like they are up, else to HALT.
% 'RUNNING'({
%         probe_return, 
%         #ps_state{probe=#probe{status ='CRITICAL'}} = NewSData,
%         PR
%     },  SData) ->
%     #ps_state{parents = Parents} = SData,
%     case parents_alive(Parents) of
%         true ->
%             NegoReturn = {probe_return, NewSData, PR},
%             NegoList   = [{Parent, unknown} || {Parent, _, _} <- Parents],
%             NewSData2  = SData#ps_state{nego_return   = NegoReturn},
%             NewSData3  = NewSData2#ps_state{nego_parents = NegoList},
%             NewSData4  = NewSData3#ps_state{force_check  = false},
%             request_parent_update(NewSData4),
%             {next_state, 'NEGOCIATE', NewSData4};
%         false ->
%             {next_state, 'HALT', NewSData4}
%     end;
% 
% % no 'CRITICAL' return, normal.
% 'RUNNING'({probe_return, NewSData, PR}, SData) ->
%     #ps_state{parents = Parents} = SData,
%     handle_probe_reply(SData, NewSData, PReturn),
%     TRef        = launch_probe(NewSData, Step),
%     NewSData1   = NewSData#ps_state{tref = TRef}
%     NewSData2   = NewSData1#ps_state{check_state = sleeping},
%     NewSData3   = NewSData2#ps_state{force_check = false},
%     NewSData4   = NewSData3#ps_state{parents = Parents},
%     {next_state, 'RUNNING', NewSData4};
% 
% % parents look like they are up, but I am 'CRITICAL'. Waiting for newest
% % parent_move to decide if it is true.
% 'NEGOCIATE'({parent_move, Parent, Status, Ts}, SData) ->
%     #ps_state{nego_parents  = NegoParents}  = SData,
%     #ps_state{nego_return   = NegoReturn}    = SData,
%     #ps_state{last_return   = Last}          = SData,
%     #ps_state{step          = Step}          = SData,
% 
%     % only take parents return newer than the last return of me:
%     case timer:now_diff(ParentReturnTS, LastReturnTS) > 0 of
%         true ->
%             NegoParents2 = lists:keystore(Parent, 1, NegoParents, {Parent, Status});
%         false ->
%             NegoParents2 = NegoParents
%     end,
% 
%     case nego_complete(NegoParents2) of
%         true  ->
%             case parents_alive(NegoParents2) of
%                 true    ->
%                     {_, NewSData, PR} = NegoReturn,
%                     handle_probe_reply(SData, NewSData, PReturn),
%                     launch_probe(NewSData, Step),
%                     TRef        = launch_probe(NewSData, Step),
%                     NewSData1   = NewSData#ps_state{tref = TRef}
%                     NewSData2   = NewSData1#ps_state{check_state = sleeping},
%                     NewSData3   = NewSData2#ps_state{force_check = false},
%                     NewSData4   = NewSData3#ps_state{parents = Parents},
%                     {next_state, 'RUNNING', NewSData4};
%                 false   ->
%                     {_, NewSData, PR}           = NegoReturn,
%                     #ps_state{probe = NewProbe} = NewSData,
%                     NewProbe2 = NewProbe#probe{status = ?HALT}, % TODO change status to HALT
%                     NewSData2 = NewSData#ps_state{probe = NewProbe2},
%                     handle_probe_reply(SData, NewSData2, PReturn),
%                     {next_state, 'HALT', NewSData2}
%                 end;
% 
%         false ->
%             NewSData = SData#ps_state{nego_parents = NegoParents2},
%             {next_state, 'NEGOCIATE', NewSData}
%     end.
%         
% % wait for parents to come up
% 'HALT'(parent_move, SData) ->
%     #ps_state{parents   = Parents}  = SData,
%     #ps_state{step      = Step}     = SData,
% 
%     case parents_alive(Parents) of
%         true ->
%             launch_probe(SData, Step),
%             {next_state, 'TAKE-OVER', SData};
%         false ->
%             {next_state, 'HALT', SData}
%     end.
% 
% % in 'TAKE-OVER' state, return in HALT state if parent come down again.
% 'TAKE-OVER'(parent_move, SData) ->
%     #ps_state{parents   = Parents}  = SData,
%     case parents_alive(Parents) of
%         true ->
%             {next_state, 'TAKE-OVER', SData};
%         false ->
%             {next_state, 'HALT', SData}
%     end.
%             
% 
% handle_event({probe_return, NewSData, PR}, SName, SData) ->
%     #ps_state{force_check = Force}     = SData
%     #ps_state{parents   = Parents}     = SData,
%     #ps_state{probe     = OldProbe}    = SData,
%     #ps_state{probe     = NewProbe}    = NewSData,
%     #probe{status       = OldStatus}   = OldProbe,
%     #probe{status       = NewStatus}   = NewProbe,
% 
%     #ps_state{step      = NormalStep}  = SData,
%     case Force of
%         true ->
%             Step = 1;
%         false ->
%             Step = NormalStep
%     end;
% 
%     case NewStatus of
%         'INITIALIZE' ->
%             {next_state, 'INITIALIZE', NewSData};
%         'OK' ->
%         'WARNING' ->
%             handle_probe_reply(SData, NewSData, PReturn),
%             TRef        = launch_probe(NewSData, Step),
%             NewSData1   = NewSData#ps_state{tref = TRef}
%             NewSData2   = NewSData1#ps_state{check_state = sleeping},
%             NewSData3   = NewSData2#ps_state{force_check = false},
%             {next_state, 'WARNING', NewSData3};
%         'UNKNOWN' ->
%             handle_probe_reply(SData, NewSData, PReturn),
%             TRef        = launch_probe(NewSData, Step),
%             NewSData1   = NewSData#ps_state{tref = TRef}
%             NewSData2   = NewSData1#ps_state{check_state = sleeping},
%             NewSData3   = NewSData2#ps_state{force_check = false},
%             {next_state, 'UNKNOWN', NewSData3};
%         'CRITICAL' ->
%             case OldStatus of
%                 'CRITICAL' -> % NEGOCIATE allready done
%                     case parents_alive(Parents) of
%                         true ->
%                             handle_probe_reply(SData, NewSData, PReturn),
%                             launch_probe(NewSData, Step),
%                             TRef        = launch_probe(NewSData, Step),
%                             NewSData1   = NewSData#ps_state{tref = TRef}
%                             NewSData2   = NewSData1#ps_state{check_state = sleeping},
%                             NewSData3   = NewSData2#ps_state{force_check = false},
%                             {next_state, 'CRITICAL', NewSData2};
%                         false ->
%                             NewProbe1 = NewProbe#probe{status = ?HALT},
%                             NewSData2 = NewSData1#ps_state{probe = NewProbe2},
%                             handle_probe_reply(SData, NewSData2, PReturn),
%                             {next_state, 'HALT', NewSData2}
%                     end;
%                 _ ->
%                     NegoReturn = {probe_return, NewSData, PR},
%                     NegoList   = [{Parent, unknown} || {Parent, _} <- Parents],
%                     NewSData2  = NewSData#ps_state{nego_return   = NegoReturn},
%                     NewSData3  = NewSData2#ps_state{nego_parents = NegoList},
%                     NewSData3  = NewSData2#ps_state{force_check  = false},
%                     request_parent_update(NewSData3),
%                     {next_state, 'NEGOCIATE', NewSData3}
%             end
%     end;
% 
% handle_event(force_check, SName, SData) ->
%     #ps_state{tref = TRef}          = SData,
%     #ps_state{probe = Probe}        = SData,
%     #ps_state{childs = Childs}  = SData,
%     #ps_state{check_status = State} = SData,
%     #prope{name = Name}             = Sdata,
%     #prope{status = Status}         = Sdata,
%     case SName of
%         'HALT' ->
%             emit_childs(Status, erlang:now(), Name, Childs),
%             {next_state, SName, SData};
%         'NEGOCIATE' ->
%             emit_childs(Status, erlang:now(), Name, Childs),
%             {next_state, SName, SData};
%         _ ->
%             case State of
%                 sleeping ->
%                     timer:cancel(TRef),
%                     TRef        = launch_probe(SData, 1),
%                     NewSData    = SData#ps_state{tref = TRef}
%                     NewSData2   = NewSData#ps_state{check_state = sleeping},
%                     {next_state, SName, NewSData2};
%                 running  ->
%                     {next_state, SName, SData#ps_state{force_check = true}
%             end
%     end.
% 
% 
% 
% % GEN_CHANNEL event
% 
% % prevent 2 probes to be launched in paralel. Possible in a race condition
% % with force_check. Stop the late check is sufficient.

% handle_sync_event({check_start, Ts}, _From, SName, SData) ->
%     #ps_state{probe_state = State} = SData,
%     case State of
%         stoped ->
%             {reply, false, SName, SData};
%         sleeping ->
%             {reply, true, SName, SData#ps_state{probe_state = running}}.
%         running  ->
%             {reply, false, SName, SData}
%     end;
% 
% % tracker_probe_sup event
% handle_sync_event(freeze, _From, SName, SData) ->
%     #ps_state{tref = TRef}           = SData,
%     timer:cancel(TRef),
%     TRef        = launch_probe(SData, 1),
%     NewSData    = SData#ps_state{tref = TRef}
%     NewSData2   = NewSData#ps_state{check_state = sleeping},
%     {next_state, 'INITIALIZE', NewSData2};
% 
%     
% % GEN_CHANNEL event
% handle_sync_event(get_perms, _From, SName, SData) ->
%     #ps_state{probe     = Probe}        = SData,
%     #probe{permissions  = Permissions}  = Probe,
%     {reply, Permissions, SName, SData}.
% 
% handle_event({sync_request, CState}, SName, SData) ->
%     #ps_state{probe     = Probe}    = SData,
%     #probe{name         = Name}     = Probe,
%     Pdus    = log_dump(SData),
%     ok      = send_unicast(CState, Pdus),
%     ok      = gen_channel:subscribe(Name, CState),
%     {next_state, SName, SData}.
% 
% emit_childs(_, _, _, []) -> ok;
% emit_childs(Status, Ts, Name, [Child|Tail]) ->
%     tracker_probe_fsm:emit_parent_move(Child, Ts, Name, Status),
%     emit_childs(Status, Ts, Name, Tail).
% 
% request_parent_update(SData) when is_tuple(SData) ->
%     #ps_state{parents = Parents} = SData,
%     %?LOG({request_parent_update, Parents}),
%     request_parent_update(Parents);
% request_parent_update([]) -> ok;
% request_parent_update([Parent|T]) ->
%     tracker_probe_fsm:force_check(Parent),
%     request_parent_update(T).
% 
% nego_complete([]) -> true;
% nego_complete([{_, stop_reachable, _} | _]) -> false;
% nego_complete([_|T]) -> nego_complete(T).
% 
% parents_alive([]) -> true;
% parents_alive(L) -> check_parents_alive(L).
% check_parents_alive([]) -> false;
% check_parents_alive([{_,Status, _}|H]) ->
%     case Status of
%         'CRITICAL'      -> check_parents_alive(H);
%         'UNKNOWN'       -> check_parents_alive(H);
%         check_reachable -> check_parents_alive(H);
%         _               -> true
%     end.
% 
% emit_status(_,         _, []) -> ok;
% emit_status(Status, Name, [Child|T]) ->
%     tracker_probe_fsm:emit_parent_move(Child, Name, Status),
%     emit_status(Status, Name, T).
% 
% update_parents({probe_return, Parent, Status, Ts}, Parents) ->
%     NewParents = lists:keystore(Parent, 1, Parents, {Parents, Status, Ts}),
%     NewParents.
% 
handle_info(_Info, SName, SData) ->
    {next_state, SName, SData}.
% 
terminate(_Reason, _SName, _SData) ->
    normal.
% 
code_change(_OldVsn, SName, SData, _Extra) ->
    {ok, SName, SData}.

% 
% 
% 
% 
% %%
% %% HANDLE PROBE REPLY
% %%
% 
% % Notify all the subscribers of the probe (probeReturn)
% 
% %%
% %%
% %%
% 
% % send to a #client_state{}
% send_unicast(CState, Pdus) ->
%     #client_state{module = Mod} = CState,
%     lists:foreach(fun(Pdu) -> Mod:send(Pdu) end, Pdus).
%
% %%
% %% UTILS 
% %%

  
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
% 
% -spec log_dump(#ps_state{}) -> any().
% log_dump(#ps_state{probe = Probe} = PSState) ->
%     L = [Mod:dump(PSState) || 
%             #logger{module = Mod} <- Probe#probe.loggers],
%     L2 = lists:filter(fun(Element) ->
%         case Element of
%             ignore  -> false;
%             _       -> true
%         end
%     end, L),
%     L2.

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

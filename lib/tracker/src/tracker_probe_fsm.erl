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
-behaviour(supercast_channel).
-include("../include/tracker.hrl").

% start
-export([
    start_link/1
]).

% supercast_channel
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
    'RUNNING-FREEZED'/2,
    'RUNNING-FREEZED'/3,
    'RUNNING'/2,
    'RUNNING'/3
    % TODO
    %'FORCE'/2,
    %'NEGOCIATE'/2
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
]).

-record(parent, {
    name,
    status,
    last_check
}).

-define(OK,         'OK').
-define(CRITICAL,   'CRITICAL').
-define(WARNING,    'WARNING').
-define(UNKNOWN,    'UNKNOWN').
-define(HALT,       'UNKNOWN').

% fsm behaviour
start_link({Target, #probe{name = Name} = Probe}) ->
    gen_fsm:start_link({local, Name}, ?MODULE, [Target, Probe], []).

% supercast_channel behaviour
get_perms(PidName) ->
    gen_fsm:sync_send_all_state_event(PidName, get_perms).

sync_request(PidName, CState) ->
    gen_fsm:send_all_state_event(PidName, {sync_request, CState}).





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
    gen_fsm:send_event(PidName, launch).



init([Target, Probe]) ->
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


'RUNNING-FREEZED'({register_child, Child}, SData) ->
    #ps_state{childs    = Childs}   = SData,
    NewChilds   = [Child|Childs],
    NewSData    = SData#ps_state{childs = NewChilds},
    {next_state, 'RUNNING-FREEZED', NewSData};
'RUNNING-FREEZED'(launch, SData) ->
    NewSData = launch_probe(SData#ps_state{check_flag = random}),
    {next_state, 'RUNNING', NewSData};

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

%% i was allready 'CRITICAL' just check parents status and go to unknown
%% if the are dead.
'RUNNING'({probe_reply, DiffSData, PR,      'CRITICAL'},
        #ps_state{probe = #probe{status =   'CRITICAL'}} = SData) ->
    #ps_state{parents = Parents} = SData,
    case parents_alive(Parents) of
        false -> % go UNKNOWN
            #ps_state{probe = DiffProbe}    = DiffSData,
            NewDiffProbe = DiffProbe#probe{status = 'UNKNOWN'},
            NewDiffSData = DiffSData#ps_state{probe = NewDiffProbe},
            NewSData1    = handle_probe_reply(SData, NewDiffSData, PR),
            NewSData2    = launch_probe(NewSData1),
            {next_state, 'RUNNING', NewSData2};
        _ ->
            NewSData1 = handle_probe_reply(SData, DiffSData, PR),
            NewSData2 = launch_probe(NewSData1),
            {next_state, 'RUNNING', NewSData2}
    end;

'RUNNING'({probe_reply, DiffSData, PR,      'CRITICAL'},
        #ps_state{probe = #probe{status =   'UNKNOWN'}} = SData) ->
    #ps_state{parents = Parents} = SData,
    case parents_alive(Parents) of
        false -> % stay UNKNOWN
            #ps_state{probe = DiffProbe}    = DiffSData,
            NewDiffProbe = DiffProbe#probe{status = 'UNKNOWN'},
            NewDiffSData = DiffSData#ps_state{probe = NewDiffProbe},
            NewSData1    = handle_probe_reply(SData, NewDiffSData, PR),
            NewSData2    = launch_probe(NewSData1),
            {next_state, 'RUNNING', NewSData2};
        _ ->
            NewSData1 = handle_probe_reply(SData, DiffSData, PR),
            NewSData2 = launch_probe(NewSData1),
            {next_state, 'RUNNING', NewSData2}
    end;

% it is the first time i am 'CRITICAL' go to negociate state if
% my parents are up.
'RUNNING'({probe_reply, DiffSData, PR, 'CRITICAL'} = _Ret, SData) ->
    #ps_state{parents = Parents} = SData,
    case parents_alive(Parents) of
        no_parents -> % no parents nothing to do
            NewSData1 = handle_probe_reply(SData, DiffSData, PR),
            NewSData2 = launch_probe(NewSData1),
            {next_state, 'RUNNING', NewSData2};
        false  -> % parents are down, I am now UNKNOWN
            #ps_state{probe = DiffProbe} = DiffSData,
            NewDiffProbe    = DiffProbe#probe{status = 'UNKNOWN'},
            NewDiffSData    = DiffSData#ps_state{probe = NewDiffProbe},
            NewSData1       = handle_probe_reply(SData, NewDiffSData, PR),
            NewSData2       = launch_probe(NewSData1),
            {next_state, 'RUNNING', NewSData2};
        true -> % parents are up, wait reply from parents to go
            #ps_state{probe = DiffProbe} = DiffSData,
            NewDiffProbe    = DiffProbe#probe{status = 'UNKNOWN'},
            NewDiffSData    = DiffSData#ps_state{probe = NewDiffProbe},
            NewSData1       = handle_probe_reply(SData, NewDiffSData, PR),
            NewSData2       = launch_probe(NewSData1),
            {next_state, 'RUNNING', NewSData2}



        % TODO NEGOCIATE
        %true ->
            % go 'UNKNOWN' for the moment and keep the probe reply
            % to maybe andle it later.
            %#ps_state{probe = Probe} = SData,
            %NewProbe        = Probe#probe{status = 'UNKNOWN'},
            %NewSData        = SData#ps_state{probe = NewProbe},
            %NewSData1       = NewSData#ps_state{nego_return = Ret},
            % put the return time of the diff probe
            %#ps_state{last_check = LC} = DiffSData,
            %NewSData2       = NewSData1#ps_state{last_check = LC},
            %NewSData3       = handle_probe_reply(SData, NewSData2, PR),
            %?LOG(go_negociate),
            %{next_state, 'NEGOCIATE', NewSData3}
    end;


'RUNNING'({probe_reply, DiffSData, PR, _S}, SData) ->
    NewSData1 = handle_probe_reply(SData, DiffSData, PR),
    NewSData2 = launch_probe(NewSData1),
    {next_state, 'RUNNING', NewSData2};

'RUNNING'({parent_move, ParentRec}, SData) ->
    #ps_state{parents = Parents} = SData,
    #parent{name = Name}         = ParentRec,
    NewParents  = lists:keystore(Name, 2, Parents, ParentRec),
    NewSData    = SData#ps_state{parents = NewParents},
    {next_state, 'RUNNING', NewSData}.

% TODO NEGOCIATE
% 'NEGOCIATE'({parent_move, ParentRec}, SData) ->
%     #ps_state{parents = Parents} = SData,
%     #parent{name = Name}         = ParentRec,
%     NewParents  = lists:keystore(Name, 2, Parents, ParentRec),
%     NewSData    = SData#ps_state{parents = NewParents},
%     ?LOG({parent_move, SData#ps_state.probe#probe.name}),
%     case parents_up_to_date(NewSData) of
%         true ->
%             ?LOG({up_to_date, true, SData#ps_state.probe#probe.name}),
%             case parents_alive(NewParents) of
%                 true    ->
%                     ?LOG({parents_alive, true, SData#ps_state.probe#probe.name}),
%                     #ps_state{nego_return = Ret} = NewSData,
%                     {_, DiffSData, PR, _} = Ret,
%                     NewSData1 = handle_probe_reply(NewSData, DiffSData, PR),
%                     NewSData2 = launch_probe(NewSData1),
%                     {next_state, 'RUNNING', NewSData2};
%                 false   ->
%                     ?LOG({parents_alive, false, SData#ps_state.probe#probe.name}),
%                     #ps_state{nego_return = Ret} = NewSData,
%                     {_, DiffSData, PR, _} = Ret,
%                     NewSData1 = handle_probe_reply(NewSData, DiffSData, PR),
%                     NewSData2 = launch_probe(NewSData1),
%                     {next_state, 'RUNNING', NewSData2}
% 
%                     %NewSData1 = launch_probe(NewSData),
%                     %{next_state, 'RUNNING', NewSData1}
%             end;
%         false ->
%             ?LOG({up_to_date, false, SData#ps_state.probe#probe.name}),
%             {next_state, 'NEGOCIATE', NewSData}
%     end.




% GEN_CHANNEL event
handle_event({sync_request, CState}, SName, SData) ->
    #ps_state{probe     = Probe}    = SData,
    #probe{name         = Name}     = Probe,
    Pdus    = log_dump(SData),
    ok      = send_unicast(CState, Pdus),
    ok      = supercast_channel:subscribe(Name, CState),
    {next_state, SName, SData};

handle_event(_, SName, SData) ->
    {next_state, SName, SData}.

handle_sync_event(get_perms, _From, SName, SData) ->
    #ps_state{probe     = Probe}        = SData,
    #probe{permissions  = Permissions}  = Probe,
    {reply, Permissions, SName, SData};


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
    
    % XXX keep the old parents it might have changed
    #ps_state{parents       = Parents}    = SData,
    NSData2 = NSData1#ps_state{parents = Parents}, 


    case ProbeS1 of
        ProbeS2 ->
            emit_childs(NSData2),
            emit_local(NSData2, PR),
            NSData2;
        _ ->
            emit_childs(NSData2),
            emit_local(NSData2, PR),
            emit_wide(NSData2, PR),
            NSData2
    end.

emit_local(SData, PR) ->
    #ps_state{probe     = Probe}    = SData,
    #ps_state{target    = Target}   = SData,
    #probe{name         = Name}     = Probe,
    #probe{permissions  = Perms}    = Probe,
    #target{id          = TargetId} = Target,
    Pdu = tracker_pdu:probe_return({PR, TargetId, Name}),
    supercast_channel:emit(Name, {Perms, Pdu}),
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
    MoveEvent = #parent{name = Name, status = Status, last_check = Ts},
    gen_fsm:send_event(Child, {parent_move, MoveEvent}),
    emit_childs(Status, Ts, Name, Tail).

parents_alive([]) -> no_parents;
parents_alive(L) -> check_parents_alive(L).
check_parents_alive([]) -> false;
check_parents_alive([{parent,_,Status, _}|H]) ->
    case Status of
        'CRITICAL'      -> check_parents_alive(H);
        'UNKNOWN'       -> check_parents_alive(H);
        _               -> true
    end.

% TODO HALT
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
% TODO TAKE-OVER
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

handle_info(_Info, SName, SData) ->
    {next_state, SName, SData}.
% 
terminate(_Reason, _SName, _SData) ->
    normal.
% 
code_change(_OldVsn, SName, SData, _Extra) ->
    {ok, SName, SData}.


%%
%% UTILS 
%%
% send to a #client_state{}
send_unicast(CState, Pdus) ->
    #client_state{module = Mod} = CState,
    lists:foreach(fun(Pdu) -> Mod:send(Pdu) end, Pdus).

  
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

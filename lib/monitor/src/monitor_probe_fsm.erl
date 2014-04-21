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
-module(monitor_probe_fsm).
-behaviour(gen_fsm).
-behaviour(supercast_channel).
-include("include/monitor.hrl").

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
    code_change/4,
    'RUNNING'/3
]).


% local api
-export([
    freeze/1,
    reset_family/1,
    synchronize_parents/1,
    synchronize_childs/1,
    launch/1,           % from monitor_probe_sup:parents_sync/0
    take_of/2
]).

%-record(parent, {
    %name,
    %status,
    %last_check
%}).

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
%% from monitor_probe_sup
%%
-spec freeze(pid()) -> ok.
% @doc
% Called by monitor_probe_sup before parents init. This function
% every actions and put the probe in 'INITIALIZE' state. If the state
% of the probe was 'WAITING-REPLY' put the probe in 
% 'INITIALIZE-WAITING-REPLY' state.
% @end
freeze(PidName) ->
    gen_fsm:sync_send_all_state_event(PidName, freeze).

-spec reset_family(pid()) -> ok.
reset_family(PidName) ->
    gen_fsm:sync_send_all_state_event(PidName, reset_family).

-spec synchronize_parents(pid()) -> ok.
% @doc
% The probe MUST be in 'INITIALIZE' state to execute this function.
% Called by monitor_probe_sup after freeze/1.
% @end
synchronize_parents(PidName) ->
    gen_fsm:sync_send_all_state_event(PidName, synchronize_parents).

synchronize_childs(PidName) ->
    gen_fsm:sync_send_all_state_event(PidName, synchronize_childs).



-spec launch(pid()) -> ok.
% @doc
% Start the probe. The probe MUST be in 'INITIALIZE' state to execute
% this function. Put the probe in 'SLEEPING-STEP' state with a random
% start from 0 to #probe.step.
% @end
launch(PidName) ->
    gen_fsm:send_event(PidName, launch).



init([Target, Probe]) ->
    init_random(),
    UProbe = Probe#probe{
        step    = Probe#probe.step    * 1000,
        timeout = Probe#probe.timeout * 1000,
        pid     = self()
    },


    %Parents                 = UProbe#probe.parents,
    %ProbeParents            = [#parent{name = Parent} || Parent <- Parents],

    {ok, ProbeInitState}    = init_probe(Target, UProbe),
    {ok, InspectInitState}  = init_inspectors(Target, UProbe),
    {ok, LoggersInitState}  = init_loggers(Target, UProbe),
    PSState = #ps_state{
        name                = UProbe#probe.name,
        target              = Target,
        probe               = UProbe,
        step                = UProbe#probe.step,
        timeout             = UProbe#probe.timeout,

    %    parents             = ProbeParents,
    %    childs              = [],
 
        probe_state         = ProbeInitState ,
        inspectors_state    = InspectInitState,
        loggers_state       = LoggersInitState
    },
    %?LOG({loggers_state, LoggersInitState}),
    initiate_start_sequence(ProbeInitState, UProbe, random),
    {ok, 'RUNNING', PSState}.

'RUNNING'(_Event, SName, SData) ->
    {next_state, SName, SData}.

% GEN_CHANNEL event
handle_event({probe_return, NewProbeState, ProbeReturn}, SName, SData) ->
    % Update probe_state
    SData1  = SData#ps_state{probe_state       = NewProbeState},

    % INSPECT, update probe and inspectors_state,
    Probe        = SData1#ps_state.probe,
    InspectState = SData1#ps_state.inspectors_state,
    {ok, NewInspectState, ModifiedProbe} = 
        inspect(InspectState, Probe, ProbeReturn),
    SData2  = SData1#ps_state{probe             = ModifiedProbe},
    SData3  = SData2#ps_state{inspectors_state  = NewInspectState},

    % NOTIFY
    Target  = SData3#ps_state.target,
    notify(ProbeReturn, Target, Probe, ModifiedProbe),

    % LOG, update loggers_state,
    LoggersState            = SData3#ps_state.loggers_state,
    {ok, NewLoggersState}   = log_return(LoggersState, ProbeReturn),
    SData4 = SData3#ps_state{loggers_state = NewLoggersState},

    % LAUNCH
    PS      = SData4#ps_state.probe_state,
    P       = SData4#ps_state.probe,
    initiate_start_sequence(PS, P, normal),

    {next_state, SName, SData4};

handle_event({emit_pdu, Pdu}, SName, SData) ->
    Probe       = SData#ps_state.probe,
    ChanName    = Probe#probe.name,
    Perms       = Probe#probe.permissions,
    supercast_channel:emit(ChanName, {Perms, Pdu}),
    {next_state, SName, SData};

handle_event({sync_request, CState}, SName, SData) ->
    Probe   = SData#ps_state.probe,
    Name    = Probe#probe.name,
    LStates = SData#ps_state.loggers_state,
    {ok, NewLStates, Pdus} = log_dump(LStates),
    ok      = supercast_channel:unicast(CState, Pdus),
    ok      = supercast_channel:subscribe(Name, CState),
    SData1  = SData#ps_state{loggers_state = NewLStates},
    {next_state, SName, SData1}.

handle_sync_event(get_perms, _From, SName, SData) ->
    Probe = SData#ps_state.probe,
    Perms = Probe#probe.permissions,
    {reply, Perms, SName, SData}.

handle_info(_Info, SName, SData) ->
    {next_state, SName, SData}.

terminate(_Reason, _SName, _SData) ->
    normal.

code_change(_OldVsn, SName, SData, _Extra) ->
    {ok, SName, SData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PROBE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_probe(Target, Probe) ->
    Mod         = Probe#probe.monitor_probe_mod,
    InitState   = Mod:init(Target, Probe),
    InitState.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOGGERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_loggers(Target, Probe) ->
    Loggers         = Probe#probe.loggers,
    LoggersState    = [],
    init_loggers(Target, Probe, Loggers, LoggersState).
init_loggers(_Target, _Probe, [], LoggersState) ->
    {ok, LoggersState};
init_loggers(Target, Probe, [Logger|Loggers], LoggersState) ->
    Mod             = Logger#logger.module,
    Conf            = Logger#logger.conf,
    {ok, State}     = Mod:init(Conf, Target, Probe),
    LoggersState2   = lists:keystore(Mod, 1, LoggersState, {Mod, State}),
    init_loggers(Target, Probe, Loggers, LoggersState2).
    
log_return(LoggersState, ProbeReturn) ->
    LoggersStateAcc = [],
    log_return(LoggersState, ProbeReturn, LoggersStateAcc).
log_return([],  _ProbeReturn, LoggersStateAcc) ->
    {ok, LoggersStateAcc};
log_return([Logger|LoggersState],  ProbeReturn, LoggersStateAcc) ->
    {Mod, State}     = Logger,
    % loggers may return a Pdu to update the client state.
    case Mod:log(State, ProbeReturn) of
        {ok, NewState}      ->
            ok;
        {ok, Pdu, NewState} ->
            gen_fsm:send_all_state_event(self(), {emit_pdu, Pdu})
    end,
    LoggersStateAcc2 = lists:keystore(Mod,1,LoggersStateAcc, {Mod, NewState}),
    log_return(LoggersState, ProbeReturn, LoggersStateAcc2).

log_dump(LoggersState) ->
    PduAcc      = [],
    NewLogState = [],
    log_dump(LoggersState, PduAcc, NewLogState).
log_dump([], Pdus, LogState) ->
    {ok, Pdus, LogState};
log_dump([LoggerState|LState], PduAcc, NLogState) ->
    {Mod, State}        = LoggerState,
    case Mod:dump(State) of
        {ok, Pdu, State2} ->
            log_dump(LState, [Pdu|PduAcc], [State2|NLogState]);
        {ignore, State2} ->
            log_dump(LState, PduAcc,       [State2|NLogState])
    end.
    
%-spec log_dump(#ps_state{}) -> any().
%log_dump(#ps_state{probe = Probe} = PSState) ->
    %L = [Mod:dump(PSState) || 
            %#logger{module = Mod} <- Probe#probe.loggers],
    %L2 = lists:filter(fun(Element) ->
        %case Element of
            %ignore  -> false;
            %_       -> true
        %end
    %end, L),
    %L2.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INSPECTORS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init_inspectors(Target::#target{}, Probe::#probe{}) ->
    {ok, InspectStates::[{atom(), any()}]}.
init_inspectors(Target, Probe) ->
    Inspectors  = Probe#probe.inspectors,
    States      = [],
    init_inspectors(Target, Probe, Inspectors, States).
init_inspectors(_, _, [], InspectStates) ->
    {ok, InspectStates};
init_inspectors(Target, Probe, [Inspector|Inspectors], InspectStates) ->
    Mod                 = Inspector#inspector.module,
    Conf                = Inspector#inspector.conf,
    {ok, InspReply}     = Mod:init(Conf, Target, Probe),
    NewInspectStates    = lists:keystore(Mod,1,InspectStates,{Mod,InspReply}),
    init_inspectors(Target, Probe, Inspectors, NewInspectStates).

-spec inspect(
        InspectStates   :: [{atom(), any()}],
        Probe           :: #probe{},
        ProbeReturn     :: #probe_return{}
    ) -> {ok, NewInspectStates::[{atom(), any()}], NewProbe::#probe{}}.
inspect(InspectStates, Probe, ProbeReturn) ->
    OrigProbe           = ModifiedProbe = Probe,
    NewIStates          = [],
    inspect(InspectStates, NewIStates, ProbeReturn, OrigProbe, ModifiedProbe).
inspect([], NewInspectStates, _, _, ModifiedProbe) ->
    {ok, NewInspectStates, ModifiedProbe};
inspect([IState|IStates], NIStates, PR, OP, MP) ->
    {Mod, State} = IState,
    {ok, NState, MProbe} = Mod:inspect(State, PR, OP, MP),
    NewNIStates = lists:keystore(Mod, 1, NIStates, {Mod, NState}),
    inspect(IStates, NewNIStates, PR, OP, MProbe).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PROBE LAUNCH %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec initiate_start_sequence(
        ProbeState  ::any(),
        Probe       ::#probe{},
        Step        :: normal | random | now | integer()
    ) -> any().
initiate_start_sequence(ProbeState, Probe, random) ->
    Step    = random:uniform(Probe#probe.step),
    initiate_start_sequence(ProbeState, Probe, Step);
initiate_start_sequence(ProbeState, Probe, normal) ->
    Step    = Probe#probe.step,
    initiate_start_sequence(ProbeState, Probe, Step);
initiate_start_sequence(ProbeState, Probe, now) ->
    Step    = 0,
    initiate_start_sequence(ProbeState, Probe, Step);
initiate_start_sequence(ProbeState, Probe, Step) ->
    timer:apply_after(Step,   ?MODULE, take_of, [ProbeState, Probe]).

take_of(ProbeState, Probe) ->
    Mod     = Probe#probe.monitor_probe_mod,
    Pid     = Probe#probe.pid,
    {ok, ProbeState2, Return}  = Mod:exec(ProbeState),
    gen_fsm:send_all_state_event(Pid, {probe_return, ProbeState2, Return}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% master_channel and supercast_channel NOTIFY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
notify(ProbeReturn, Target, OriginalProbe, NewProbe) ->
    ok = notify_subscribers(ProbeReturn, Target, NewProbe),
    ok = notify_master(Target, OriginalProbe, NewProbe, ProbeReturn).

notify_subscribers(ProbeReturn, Target, Probe) ->
    ChanName = ProbeName = Probe#probe.name,
    Perms       = Probe#probe.permissions,
    TargetId    = Target#target.id,
    Pdu         = monitor_pdu:probe_return({ProbeReturn, TargetId, ProbeName}),
    supercast_channel:emit(ChanName, {Perms, Pdu}).

notify_master(Target, OriginalProbe, Probe, ProbeReturn) ->
    case notify_master_required(OriginalProbe, Probe) of
        true ->
            TargetId = Target#target.id,
            ProbeId  = Probe#probe.id,
            monitor_target_channel:update(
                TargetId, ProbeId, {Probe, ProbeReturn}
            );
        false ->
            ok
    end.

notify_master_required(Orig, Modified) ->
    OriState    = Orig#probe.status,
    ModState    = Modified#probe.status,
    OriProp     = Orig#probe.properties,
    ModProp     = Modified#probe.properties,

    case OriState of
        ModState ->
            case OriProp of
                ModProp -> false;
                _       -> true
            end;
        _ ->
            true
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTILS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_random() ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

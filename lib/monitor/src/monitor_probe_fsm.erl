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
-include("../include/monitor.hrl").

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
    'RUNNING-FREEZED'/3
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
    UProbe = Probe#probe{
        step    = Probe#probe.step    * 1000,
        timeout = Probe#probe.timeout * 1000,
        pid     = self()
    },
    Parents                 = UProbe#probe.parents,
    _Dir                    = Target#target.directory,
    ProbeParents            = [#parent{name = Parent} || Parent <- Parents],
    ProbeInitState          = init_probe(Target, UProbe),
    %{ok, _LoggersInitState}  = init_loggers(Probe, Dir),
    %{ok, InspectInitState}  = init_inspectors(Probe),
    PSState = #ps_state{
        name                = UProbe#probe.name,
        target              = Target,
        probe               = UProbe,
        step                = UProbe#probe.step,
        timeout             = UProbe#probe.timeout,
        parents             = ProbeParents,
        childs              = [],
        probe_state         = ProbeInitState ,
        %inspectors_state    = InspectInitState,
        %loggers_state       = LoggersInitState,
        loggers_state       = []
        
    },
    initiate_start_sequence(ProbeInitState, UProbe, random),
    {ok, 'RUNNING-FREEZED', PSState}.

'RUNNING-FREEZED'(_Event, SName, SData) ->
    {next_state, SName, SData}.

% GEN_CHANNEL event
handle_event({probe_return, PReturn}, SName, SData) ->
    ?LOG({probe_return, PReturn}),
    ProbeState  = SData#ps_state.probe_state,
    Probe       = SData#ps_state.probe,
    initiate_start_sequence(ProbeState, Probe),
    {next_state, SName, SData};

handle_event(_, SName, SData) ->
    {next_state, SName, SData}.

handle_sync_event(_Any, _From, SName, SData) ->
    {reply, ok, SName, SData}.

handle_info(_Info, SName, SData) ->
    {next_state, SName, SData}.

terminate(_Reason, _SName, _SData) ->
    normal.

code_change(_OldVsn, SName, SData, _Extra) ->
    {ok, SName, SData}.

% INIT PROBE
init_probe(Target, Probe) ->
    #probe{monitor_probe_mod = Mod} = Probe,
    {ok, InitState}                 = Mod:init(Target, Probe),
    InitState.

% INIT LOGGERS
% init_loggers(Probe, Dir) ->
%     #probe{loggers = Loggers} = Probe,
%     init_loggers(Probe, Dir, Loggers, []).
% init_loggers(_, _, [], InitLoggersState) ->
%     {ok, InitLoggersState};
% init_loggers(Probe, Dir, [Logger|Rest], State) ->
%     #logger{module  = Mod}  = Logger,
%     #logger{conf    = Conf} = Logger,
%     {ok, Result}    = Mod:init(Conf, Dir, Probe),
%     State2          = lists:keystore(Mod, 1, State, {Mod, Result}),
%     init_loggers(Probe, Dir, Rest, State2).
    
% TODO send conf here
%-spec log(#ps_state{}, {atom(), any()}) -> ok.
%log(#ps_state{probe = Probe} = PSState, Msg) ->
    %lists:foreach(fun(#logger{module = Mod}) ->
        %spawn(fun() -> Mod:log(PSState, Msg) end)
    %end, Probe#probe.loggers),
    %ok.

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

% INSPECTORS
% init_inspectors(Probe) ->
%     Inspectors = Probe#probe.inspectors,
%     init_inspectors(Probe, Inspectors, []).
% init_inspectors(_, [], InspectState) ->
%     {ok, InspectState};
% init_inspectors(Probe, [Inspector|Rest], InspectState) ->
%     Mod                 = Inspector#inspector.module,
%     Conf                = Inspector#inspector.conf,
%     {ok, InspReply}     = Mod:init(Conf, Probe),
%     NewInspectState     = lists:keystore(Mod,1,InspectState,{Mod,InspReply}),
%     init_inspectors(Probe, Rest, NewInspectState).


% TODO send conf here
% -spec inspect(#ps_state{}, Msg::tuple()) -> #ps_state{}.
% inspect(#ps_state{probe = Probe} = State, Msg) ->
%     Inspectors = Probe#probe.inspectors,
%     {State, NewState, Msg} = lists:foldl(
%         fun(#inspector{module = Mod}, {Orig, Modified, Message}) ->
%             {ok, New} = Mod:inspect(Orig, Modified, Message),
%             {Orig, New, Message}
%         end, {State, State, Msg}, Inspectors),
%     NewState.
%

initiate_start_sequence(ProbeState, Probe, random) ->
    Step    = Probe#probe.step,
    Random  = random(Step),
    timer:apply_after(Random, ?MODULE, take_of, [ProbeState, Probe]);
initiate_start_sequence(ProbeState, Probe, Step) ->
    timer:apply_after(Step, ?MODULE, take_of, [ProbeState, Probe]).

initiate_start_sequence(ProbeState, Probe) ->
    Step = Probe#probe.step,
    timer:apply_after(Step, ?MODULE, take_of, [ProbeState, Probe]).

take_of(ProbeState, Probe) ->
    Mod     = Probe#probe.monitor_probe_mod,
    Pid     = Probe#probe.pid,
    Return  = Mod:exec(ProbeState, Probe),
    gen_fsm:send_all_state_event(Pid, {probe_return, Return}).

random(Step) ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),
    random:uniform(Step).

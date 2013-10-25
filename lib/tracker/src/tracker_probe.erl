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
-module(tracker_probe).
-behaviour(gen_server).
-include("../include/tracker.hrl").

-export([
    init/1,
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

-export([
    start_link/1,
    cold_start/1,
    loggers_update/2,
    probe_pass/1,
    dump/1
]).


%%-------------------------------------------------------------
%%-------------------------------------------------------------
%% API
%%-------------------------------------------------------------
%%-------------------------------------------------------------

start_link({Target, Probe}) ->
    gen_server:start_link(?MODULE, [Target, Probe], []).

-spec cold_start(pid()) -> ok.
% @doc
% Once the server is started, it need to be initialized so he can then enter
% in an infinite loop. This function is called from his parent 
% tracker_target_channel() module.
% @end
cold_start(Pid) ->
    gen_server:call(Pid, initial_pass).


-spec loggers_update(pid(), any()) -> ok.
% @doc
% The server who synchronise subscription is the tracker_target_channel. It
% is him who decide to notify loggers of a message.
% @end
loggers_update(Pid, Msg) ->
    gen_server:cast(Pid, {loggers_update, Msg}).

-spec dump(pid()) -> [any()].
% @doc
% from the _client -> traget_target_channel -> tracker_probe
% @end
dump(Pid) ->
    gen_server:call(Pid, probe_dump).
%%-------------------------------------------------------------
%%-------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%-------------------------------------------------------------
%%-------------------------------------------------------------

%%-------------------------------------------------------------
%% INIT
%%-------------------------------------------------------------
init([Target, Probe]) ->
    S1  = #probe_server_state{
        target              = Target,
        probe               = Probe#probe{pid = self()},
        loggers_state       = [],
        inspectors_state    = []
    },
    {ok, S2}    = init_loggers(S1),
    {ok, S3}    = init_inspectors(S2),
    {ok, S3}.
        
    
%%-------------------------------------------------------------
%% HANDLE_CALL
%%-------------------------------------------------------------

% launch a probe for the first time. Give a way for inspectors and loggers
% to initialize themself and Randomise start.
handle_call(initial_pass, _, #probe_server_state{probe = Probe} = S) ->
    Step            = Probe#probe.step, 
    InitialLaunch   = tracker_misc:random(Step * 1000),
    timer:apply_after(InitialLaunch, ?MODULE, probe_pass, [S]),
    {reply, ok, S};

handle_call(probe_dump, _, S) ->
    Rep = log_dump(S),
    {reply, Rep, S};

handle_call(_R, _F, S) ->
    {noreply, S}.


%%-------------------------------------------------------------
% HANDLE_CAST
%%-------------------------------------------------------------

% if Status and Properties are equal then the probe have moved nothing.
% XXX: will need to notify target_channel to update loggers.
handle_cast({next_pass, 
        #probe_server_state{
            probe  = #probe{status          = Status},
            target = #target{properties     = SysProp}},
        ProbeReturn
        },
        #probe_server_state{
            probe  = #probe{status          = Status}   = Probe,
            target = #target{properties     = SysProp}  = Target} = S) ->
    tracker_target_channel:update(
        Target#target.id,
        Probe#probe.id,
        {local_event, ProbeReturn}
    ),
    After = Probe#probe.step * 1000,
    timer:apply_after(After, ?MODULE, probe_pass, [S]),
    {noreply, S};

% else notify the parent channel of property or status modification
handle_cast({next_pass, 
        #probe_server_state{probe = NProbe, target = NTarget} = NewS,
        ProbeReturn
        },
        #probe_server_state{probe = Probe,  target = Target}  = _OldS) ->
    CurrentStatus   = Probe#probe.status,
    NewStatus       = NProbe#probe.status,
    case NewStatus of
        CurrentStatus   -> ignore;
        OtherStatus     -> 
            tracker_target_channel:update(
                Target#target.id,
                Probe#probe.id,
                {broad_event, 
                    {probe_status_move, OtherStatus, ProbeReturn}}
            )
    end,
    CurrentSysP     = Target#target.properties,
    NewSysP         = NTarget#target.properties,
    case NewSysP of
        CurrentSysP     -> ignore;
        OtherSysP       ->
            tracker_target_channel:update(
                Target#target.id,
                Probe#probe.id,
                {broad_event, {property_set, OtherSysP, ProbeReturn}}
            )
    end,
    After = NProbe#probe.step * 1000,
    timer:apply_after(After, ?MODULE, probe_pass, [NewS]),
    {noreply, NewS};

handle_cast({loggers_update, Msg}, S) ->
    log(S,Msg),
    {noreply, S};

handle_cast(_R, S) ->
    io:format("Unknown message ~p ~p ~p ~p~n", [?MODULE, ?LINE, _R, S]),
    {noreply, S}.


%%-------------------------------------------------------------
%% HANDLE_INFO
%%-------------------------------------------------------------
handle_info(I, S) ->
    io:format("handle_info ~p ~p ~p~n", [?MODULE, I, S]),
    {noreply, S}.


%%-------------------------------------------------------------
%% TERMINATE  
%%-------------------------------------------------------------
terminate(_R, _) ->
    normal.


%%-------------------------------------------------------------
%% CODE_CHANGE
%%-------------------------------------------------------------
code_change(_O, S, _E) ->
    io:format("code_change ~p ~p ~p ~p~n", [?MODULE, _O, _E, S]),
    {ok, S}.



%%-------------------------------------------------------------
%%-------------------------------------------------------------
%% PRIVATE FUNS
%%-------------------------------------------------------------
%%-------------------------------------------------------------
-spec probe_pass(#probe_server_state{}) -> ok.
% @doc
% It is the spawned proc who call the "gen_probe" module defined in the 
% #probe{} record. Called from "initial_pass" and "next_pass" modules.
% @end
probe_pass(#probe_server_state{target = Target, probe  = Probe } = S) ->
    Mod         = Probe#probe.tracker_probe_mod,
    ProbeReturn = Mod:exec({Target, Probe}),

    % tracker_target_channel is the processus wich synchronize
    % with the client. Thus, it is in his gen_server loop that
    % a write of Result will occur. He will do this by calling
    % tracker_probe:loggers_update/x.
    % Subscribers of the target_channel will also recive this
    % Result message.
    % NOTE: tracker_target_channel will forward the lock to the loggers,
    % and continue. The client will only lock himself wile he synchronise,
    % because loggers themself will spawn the process of sync and 
    % then continue to fill the client process of update. Client will 
    % treat them after sync.

    NewS        = inspect(S, ProbeReturn),

    %io:format("result is ~p~n", [ProbeReturn]),
    % probe return is lost here (ProbeReturn) but will be needed by 
    % target_channel to update loggers.

    next_pass(NewS, ProbeReturn).

-spec next_pass(#probe_server_state{}, #probe_return{}) -> ok.
% @doc
% called from ?MODULE:probe_pass which trigger another probe_pass.
% @end
next_pass(#probe_server_state{probe = Probe} = State, ProbeReturn) ->
    gen_server:cast(Probe#probe.pid, {next_pass, State, ProbeReturn}).
 
 
% LOGGERS
-spec init_loggers(#probe_server_state{}) -> #probe_server_state{}.
init_loggers(#probe_server_state{probe = Probe} = State) ->
    NewState = lists:foldl(
        fun(#logger{module = Mod, conf = Conf}, PSState) ->
            {ok, NPSState} = Mod:init(Conf, PSState),
            NPSState
        end, State, Probe#probe.loggers),
    {ok, NewState}.

-spec log(#probe_server_state{}, {atom(), any()}) -> ok.
log(#probe_server_state{probe = Probe} = PSState, Msg) ->
    lists:foreach(fun(#logger{module = Mod}) ->
        spawn(fun() -> Mod:log(PSState, Msg) end)
    end, Probe#probe.loggers),
    ok.

-spec log_dump(#probe_server_state{}) -> any().
log_dump(#probe_server_state{probe = Probe} = PSState) ->
    L = [{Probe#probe.permissions, Mod:dump2(PSState)} || 
            #logger{module = Mod} <- Probe#probe.loggers],
    L.

% INSPECTORS
-spec init_inspectors(#probe_server_state{}) -> #probe_server_state{}.
init_inspectors(#probe_server_state{probe = Probe} = State) ->
    Inspectors = Probe#probe.inspectors,
    NewState = lists:foldl(fun(#inspector{module = Mod, conf = Conf}, S) ->
        {ok, NewState} = Mod:init(Conf, S),
        NewState 
    end, State, Inspectors),
    {ok, NewState}.

-spec inspect(#probe_server_state{}, Msg::tuple()) -> #probe_server_state{}.
inspect(#probe_server_state{probe = Probe} = State, Msg) ->
    Inspectors = Probe#probe.inspectors,
    {State, NewState, Msg} = lists:foldl(
        fun(#inspector{module = Mod}, {Orig, Modified, Message}) ->
            {ok, New} = Mod:inspect(Orig, Modified, Message),
            {Orig, New, Message}
        end, {State, State, Msg}, Inspectors),
    NewState.

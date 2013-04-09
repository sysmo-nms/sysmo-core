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
    probe_pass/1
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
    gen_server:call(Pid, initial_start).


-spec loggers_update(pid(), any()) -> ok.
loggers_update(Pid, Msg) ->
    gen_server:cast(Pid, {loggers_update, Msg}).

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
handle_call(initial_start, _, #probe_server_state{probe = Probe} = S) ->
    Step            = Probe#probe.step, 
    InitialLaunch   = tracker_misc:random(Step * 1000),
    timer:apply_after(InitialLaunch, ?MODULE, probe_pass, [S]),
    {reply, ok, S};

handle_call(_R, _F, S) ->
    {noreply, S}.


%%-------------------------------------------------------------
% HANDLE_CAST
%%-------------------------------------------------------------

% if Status and Properties are equal then the probe have moved nothing.
handle_cast({next_pass, 
        #probe_server_state{
            probe  = #probe{status          = Status},
            target = #target{properties     = SysProp} } },
        #probe_server_state{
            probe  = #probe{status          = Status}   = Probe,
            target = #target{properties     = SysProp}} = S) ->
    After = Probe#probe.step * 1000,
    timer:apply_after(After, ?MODULE, probe_pass, [S]),
    {noreply, S};

% else notify the parent channel of a modification
handle_cast({next_pass, 
        #probe_server_state{probe = NProbe, target = NTarget} = NewS},
        #probe_server_state{probe = Probe,  target = Target}  = _OldS) ->
    CurrentStatus   = Probe#probe.status,
    NewStatus       = NProbe#probe.status,
    case NewStatus of
        CurrentStatus   -> ignore;
        OtherStatus     -> 
            tracker_target_channel:update(
                Target#target.id,
                Probe#probe.id,
                {master_event, {probe_status_move, OtherStatus}}
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
                {master_event, {property_set, OtherSysP}}
            )
    end,
    After = NProbe#probe.step * 1000,
    timer:apply_after(After, ?MODULE, probe_pass, [NewS]),
    {noreply, NewS};

handle_cast({loggers_update, Msg}, S) ->
    loggers_log(S,Msg),
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
% #probe{} record.
% @end
probe_pass(#probe_server_state{target = Target, probe  = Probe } = S) ->
    Mod         = Probe#probe.tracker_probe_mod,
    Result      = Mod:exec({Target, Probe}),

    % tracker_target_channel is the processus wich synchronize
    % with the client. Thus, it is in his gen_server loop that
    % a write of Result will occur. He will do this by calling
    % tracker_probe:loggers_update/x.
    tracker_target_channel:update(
        Target#target.id,
        Probe#probe.id,
        {channel_event, Result}
    ),

    NewS        = inspect(S, Result),
    next_pass(NewS).

-spec next_pass(#probe_server_state{}) -> ok.
% @doc
% called from ?MODULE:probe_pass which trigger another probe_pass.
% @end
next_pass(#probe_server_state{probe = Probe} = State) ->
    gen_server:cast(Probe#probe.pid, {next_pass, State}).
 
 
-spec init_loggers(#probe_server_state{}) -> #probe_server_state{}.
init_loggers(#probe_server_state{probe = Probe} = State) ->
    NewState = lists:foldl(
        fun(#logger{module = Mod, conf = Conf}, PSState) ->
            {ok, NPSState} = Mod:init(Conf, PSState),
            NPSState
        end, State, Probe#probe.loggers),
    {ok, NewState}.

-spec loggers_log(#probe_server_state{}, {atom(), any()}) -> ok.
loggers_log(#probe_server_state{probe = Probe} = PSState, Msg) ->
    % will not wait return
    lists:foreach(fun(#logger{module = Mod}) ->
        spawn(fun() -> Mod:log(PSState, Msg) end)
    end, Probe#probe.loggers),
    ok.

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

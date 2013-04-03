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
    launch/1,
    probe_pass/1
]).

%%-------------------------------------------------------------
%% API
%%-------------------------------------------------------------
start_link({Target, Probe}) ->
    gen_server:start_link(?MODULE, [Target, Probe], []).

% @private
-spec launch(pid()) -> ok.
% @doc
% Once the server is started, it need to be initialized so he can then enter
% in an infinite loop. This function is called from his parent 
% tracker_target_channel() module.
% @end
launch(Pid) ->
    gen_server:call(Pid, start).


%-spec handle_event(#probe{}) -> ok.
% @doc
% Called from an inspector if an event occur.
% @end
%handle_event(Probe, Msg) ->
 %   gen_server:call(Probe#probe.pid, {handle_event, Msg}).

%%-------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%-------------------------------------------------------------
% @private
init([Target, Probe]) ->
    State = #probe_server_state{
        target = Target,
        probe  = Probe#probe{pid = self()}
    },
    {ok, State}.
        
    
% launch a probe for the first time. Give a way to inspectors to initialize
% themself and Randomise start.
% @private
handle_call(start, _F, #probe_server_state{probe = Probe} = S) ->
    Step = Probe#probe.step, 
    InitialLaunch = tracker_misc:random(Step * 1000),
    timer:apply_after(InitialLaunch, ?MODULE, 
            probe_pass, [S]),
    {reply, ok, init_inspect(S)};

handle_call(_R, _F, S) ->
    {noreply, S}.


%%-------------------------------------------------------------
% HANDLE_CAST
%%-------------------------------------------------------------
% if status are identical do nothing
handle_cast({next_pass, 
        #probe_server_state{probe = #probe{status = Status}  = Probe} = NewS},
        #probe_server_state{probe = #probe{status = Status}} = _S) ->
    After = Probe#probe.step * 1000,
    timer:apply_after(After, ?MODULE, probe_pass, [NewS]),
    {noreply, NewS};

% else notfy the parent channel
handle_cast({next_pass, #probe_server_state{probe = Probe} = NewState}, _S) ->
    io:format("status has moved to ~p~n", [Probe#probe.status]),
    % TODO notify channel
    After = Probe#probe.step * 1000,
    timer:apply_after(After, ?MODULE, probe_pass, [NewState]),
    {noreply, NewState};

handle_cast(_R, S) ->
    io:format("Unknown message ~p ~p ~p ~p~n", [?MODULE, ?LINE, _R, S]),
    {noreply, S}.

% OTHER
% @private
handle_info(I, S) ->
    io:format("handle_info ~p ~p ~p~n", [?MODULE, I, S]),
    {noreply, S}.



% @private
% terminate(R, #probe_server_state{target_chan = ChanPid}) ->
    % tracker_target_channel:new_event(ChanPid, self(), {down, R}),
    % normal.

terminate(_R, _) ->
    normal.

% @private
code_change(_O, S, _E) ->
    io:format("code_change ~p ~p ~p ~p~n", [?MODULE, _O, _E, S]),
    {ok, S}.



%%-------------------------------------------------------------
%% PRIVATE FUNS
%%-------------------------------------------------------------
% @private
-spec probe_pass(#probe_server_state{}) -> ok.
% @doc
% It is the spawned proc who call the "gen_probe" module defined in the 
% #probe{} record.
% @end
probe_pass(#probe_server_state{target = Target, probe  = Probe } = S) ->
    Mod     = Probe#probe.tracker_probe_mod,
    Result  = Mod:exec({Target, Probe}),
    io:format("result is ~p~n", [Result]),
    % TODO notify channel
    %tracker_target_channel:new_event(
        %Target#target.id,
        %Probe#probe.pid,
        %Result
    %),
    NewS    = inspect(S, Result),
    next_pass(NewS).

-spec next_pass(#probe_server_state{}) -> ok.
% @doc
% called from ?MODULE:probe_pass which trigger another probe_pass.
% @end
next_pass(#probe_server_state{probe = Probe} = State) ->
    gen_server:cast(Probe#probe.pid, {next_pass, State}).
 
 
% @private
-spec init_inspect(#probe_server_state{}) -> #probe_server_state{}.
init_inspect(#probe_server_state{probe = Probe} = State) ->
    Inspectors = Probe#probe.inspectors,
    NewState = lists:foldl(fun(#inspector{module = Mod, conf = Conf}, S) ->
        {ok, NewState} = Mod:init(Conf, S),
        NewState 
    end, State, Inspectors),
    NewState.

% @private
-spec inspect(#probe_server_state{}, Msg::tuple()) -> #probe_server_state{}.
inspect(#probe_server_state{probe = Probe} = State, Msg) ->
    Inspectors = Probe#probe.inspectors,
    {State, NewState, Msg} = lists:foldl(
        fun(#inspector{module = Mod}, {Orig, Modified, Message}) ->
            {ok, New} = Mod:inspect(Orig, Modified, Message),
            {Orig, New, Message}
        end, {State, State, Msg}, Inspectors),
    NewState.

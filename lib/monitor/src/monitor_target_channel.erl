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
% launch/remove/add probes and switch messages. 
% <p>He is reponsible of
% <ul>
%   <li>switch probe messages (probe_evt) to the related modules depending 
%   on his type (activity, rrdmessages).</li>
%   <li>keep a clean state of target events for supercast when a client subscribe
%   to this channel</li>
%   <li> add, remove probes</li>
%   <li> keep his monitor_target_store record in sync</li>
%   <li> notify monitor_master_channel of general status wich allow non
%   subscribers of the channel to have basic informations (ex: status)</li>
% </ul>
% </p>
% <p>
%   A client subscribing to this channel must also be registered to 
%   'target-MasterChannel' to receive ALL events produced by the channel.
% </p>
% @end
-module(monitor_target_channel).
-behaviour(gen_server).
-include("include/monitor.hrl").

% GEN_SERVER CALLBACKS
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3]).

% API
-export([
    start_link/1,
    update/3,
    dump/1
]).

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
start_link(#target{id = Id} = Target) ->
    gen_server:start_link({local, Id}, ?MODULE, [Target], []).

-spec update(Self::pid(), integer(), Msg::tuple()) -> ok.
% @doc
% Called by one of the monitor_probes belonging to the monitor_target_channel
% identified by Chan.
% A monitor_probe will call this function at probe status or property change.
% The message will be forwarded to the clients of master_channel.
% @end
update(Chan, ProbeId, Message) ->
    gen_server:cast(Chan, {update, ProbeId, Message}).

% @doc
% DEBUG function
% @end
dump(Id) ->
    gen_server:call(Id, dump).


%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% GEN_SERVER CALLBACKS 
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% INIT       
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
init([Target]) ->
    ok = init_dir(Target),
    {ok, TargetA}   = init_probes(Target),
    ok = monitor_master_channel:chan_add(TargetA),
    {ok, TargetA}.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% HANDLE_CALL
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------


%%----------------------------------------------------------------------------
%% SELF API CALLS 
%%----------------------------------------------------------------------------


%%----------------------------------------------------------------------------
%% CALLS VIA GEN_CHANNEL BEHAVIOUR
%%----------------------------------------------------------------------------
handle_call(dump, _F, S) ->
    {reply, S, S};

handle_call(_R, _F, S) ->
    {noreply, S}.


%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% HANDLE_CAST
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
% update (status or property), will be forwarded to the subscribers of
% 'target-MasterChannel'.
handle_cast({update, ProbeId, {NewProbe, _ProbeReturn}},
            #target{
                probes = Probes
            } = S) ->
    NProbes     = lists:keyreplace(ProbeId, 2, Probes, NewProbe),
    NewTarget   = S#target{probes = NProbes},
    monitor_master_channel:probe_update(NewTarget, NewProbe),
    {noreply, NewTarget};

handle_cast(_R, S) ->
    io:format("unknown cast ~p~n", [_R]),
    {noreply, S}.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% HANDLE_INFO
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
handle_info(_I, S) ->
    {noreply, S}.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% TERMINATE  
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
terminate(R, State) ->
    ok = monitor_master_channel:chan_del(State),
    R.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% CODE_CHANGE
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
code_change(_O, S, _E) ->
    {ok, S}.





%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% PRIVATE
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
% @private
init_dir(#target{directory = Dir}) ->
    case file:read_file_info(Dir) of
        {ok, _} ->
            ok;
        {error, enoent} ->
            file:make_dir(Dir);
        Other ->
            {error, Other}
    end.

% @private
init_probes(#target{probes = Probes} = Target) ->
    ProbesF = lists:foldl(fun(Probe, Accum) ->
        {ok, Pid} = monitor_probe_sup:new({Target, Probe}),
        [Probe#probe{pid = Pid} | Accum]
    end, [], Probes),
    {ok, Target#target{probes = ProbesF}}.


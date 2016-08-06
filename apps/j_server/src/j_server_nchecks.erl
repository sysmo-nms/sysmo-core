%%==============================================================================
%% Sysmo NMS Network Management and Monitoring solution (http://www.sysmo.io)
%%
%% Copyright (c) 2012-2016 Sebastien Serre <ssbx@sysmo.io>
%%
%% Sysmo NMS is free software: you can redistribute it and/or modify it under
%% the terms of the GNU General Public License as published by the Free Software
%% Foundation, either version 3 of the License, or (at your option) any later
%% version.
%%
%% Sysmo NMS is distributed in the hope that it will be useful, but WITHOUT ANY
%% WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
%% A PARTICULAR PURPOSE. See the GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License along with
%% Sysmo.  If not, see <http://www.gnu.org/licenses/>.
%%==============================================================================

-module(j_server_nchecks).
-include("nchecks.hrl").
-include_lib("common_hrl/include/logs.hrl").
-behaviour(gen_server).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-export([start_link/0]).

% API
-export([check/3, helper/2]).

-record(java_node, {name, pid}).
-record(state, {java_nodes=[]}).

-define(CALL_TIMEOUT, 15000).

%% TODO implement node weight concept

helper(Class, Props) ->
    gen_server:call(?MODULE, {call_nchecks,
                {helper, {Class, Props}}}, ?CALL_TIMEOUT).

check(Class, ArgList, Opaque) ->
    gen_server:call(?MODULE, {call_nchecks,
                {check, {Class, ArgList, Opaque}}}, ?CALL_TIMEOUT).


% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% GEN_SERVER
% @private
init([]) ->
    JavaPid = j_server:get_pid(nchecks),
    JavaNode = #java_node{name='local',pid=JavaPid},
    ?LOG_INFO("success pid", JavaNode),
    {ok, #state{java_nodes=[JavaNode]}}.

% CALL
% @private
handle_call({call_nchecks, {Command, Payload}}, From,
        #state{java_nodes = JavaNodes} = S) ->
    Node = lists:last(JavaNodes),
    JavaNodes2 = lists:droplast(JavaNodes),
    JavaNodes3 = [Node|JavaNodes2],
    ?LOG_INFO("Calling nchecks node", {node, Node}),
    Node#java_node.pid ! {Command, From, Payload},
    {noreply, S#state{java_nodes = JavaNodes3}}.


% CAST
% @private
handle_cast(_Cast ,S) ->
    ?LOG_INFO("Received handle cast: ", _Cast),
    {noreply, S}.


% INFO
% @private
handle_info({reply, From, Reply}, S) ->
    gen_server:reply(From, Reply),
    {noreply, S};

handle_info({worker_available, NodeName, MainMbox, NchecksMbox, _Weight} ,S) ->
    _R = erlang:monitor_node(NodeName, true),
    ?LOG_INFO("Begin to monitor node", {NodeName, _R}),
    L2 = [#java_node{name=NodeName,pid=NchecksMbox} |S#state.java_nodes],
    MainMbox ! worker_ack,
    {noreply, S#state{java_nodes=L2}};

handle_info(stop, S) ->
    ?LOG_INFO("Received stop"),
    {noreply, S};

handle_info({'EXIT', _Pid, Reason}, S) ->
    ?LOG_WARNING("EXIT with reason:", Reason),
    {stop, Reason, S};

handle_info({nodedown, NodeName}, S) ->
    ?LOG_INFO("Received nodedown:", NodeName),
    L2 = lists:keydelete(NodeName, 2, S#state.java_nodes),
    {noreply, S#state{java_nodes=L2}};

handle_info(_I, S) ->
    ?LOG_INFO("Received handle info:", _I),
    {noreply, S}.


% TERMINATE
% @private
terminate(_,_) ->
    ok.


% CHANGE
% @private
code_change(_,S,_) ->
    {ok, S}.

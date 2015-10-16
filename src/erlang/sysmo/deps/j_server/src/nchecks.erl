% Copyright (C) 2014, Sebastien Serre <sserre.bx@gmail.com>
%
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included in all
% copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
% THE SOFTWARE.

-module(nchecks).
-include("include/nchecks.hrl").
-include_lib("common_hrl/include/logs.hrl").
-behaviour(gen_server).

% gen_server
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    start_link/0
]).

% API
-export([
    check/3,
    helper/3
]).

-record(java_node, {name, pid}).
-record(state, {java_nodes=[]}).

-define(CALL_TIMEOUT, 15000).

%% TODO implement node weight concept

helper(Class, Id, Props) ->
    gen_server:call(?MODULE, {call_nchecks,
                {helper, {Class, Id, Props}}}, ?CALL_TIMEOUT).

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

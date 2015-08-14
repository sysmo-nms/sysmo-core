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

-record(state, {
    nchecks_pid     = undefined,
    replies_waiting = [],
    assert_init     = undefined
}).

-define(ASSERT_TIMEOUT, 5000).
-define(CHECK_TIMEOUT, 15000).

helper(Class, Id, Props) ->
    gen_server:call(?MODULE, {call_nchecks,
                {helper, {Class, Id, Props}}}, ?CHECK_TIMEOUT).

check(Class, ArgList, Opaque) ->
    gen_server:call(?MODULE, {call_nchecks,
                {check, {Class, ArgList, Opaque}}}, ?CHECK_TIMEOUT).


% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% GEN_SERVER
% @private
init([]) ->
    {ok, #state{}}.

% CALL
% @private
handle_call({call_nchecks, {Command, Payload}}, From,
        #state{nchecks_pid = NChecks, replies_waiting = RWait} = S) ->
    NChecks ! {Command, From, Payload},
    {noreply, S#state{replies_waiting = [From|RWait]}}.


% CAST
% @private
handle_cast(_,S) ->
    {noreply, S}.


% INFO
% @private
handle_info(stop, S) ->
    ?LOG_INFO("Received stop"),
    {noreply, S};

handle_info({reply, From, Reply}, #state{replies_waiting = RWait} = S) ->
    gen_server:reply(From, Reply),
    {noreply,
        S#state{
            replies_waiting = lists:delete(From, RWait)
        }
    };

handle_info({'EXIT', Pid, Reason}, #state{nchecks_pid = Pid} = S) ->
    ?LOG_WARNING("EXIT with reason:", Reason),
    {stop, Reason, S};

handle_info(I, S) ->
    ?LOG_INFO("Received handle info:", I),
    {noreply, S}.


% TERMINATE
% @private
terminate(_,_) ->
    ok.


% CHANGE
% @private
code_change(_,S,_) ->
    {ok, S}.

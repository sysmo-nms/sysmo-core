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
    start_link/0,
    assert_init/0
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
    io:format("call nechecks 2~n"),
    gen_server:call(?MODULE, {call_nchecks,
                {helper, {Class, Id, Props}}}, ?CHECK_TIMEOUT).

check(Class, ArgList, Opaque) ->
    gen_server:call(?MODULE, {call_nchecks,
                {check, {Class, ArgList, Opaque}}}, ?CHECK_TIMEOUT).

% @private
% @doc
% Called by start_link to ensure initialisation before returning.
% @end
assert_init() ->
    gen_server:call(?MODULE, assert_init, ?ASSERT_TIMEOUT).

% @private
start_link() ->
    Ret = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    ok = assert_init(),
    Ret.

% GEN_SERVER
% @private
init([]) ->
    process_flag(trap_exit, true),
    gen_server:cast(?MODULE, boot),
    {ok, #state{}}.

% CALL
% @private
handle_call(assert_init, F, #state{nchecks_pid = undefined} = S) ->
    {noreply, S#state{assert_init = F}};
handle_call(assert_init, _F, S) ->
    {reply, ok, S};

handle_call({call_nchecks, {Command, Payload}}, From,
        #state{nchecks_pid = NChecks, replies_waiting = RWait} = S) ->
    NChecks ! {Command, From, Payload},
    {noreply, S#state{replies_waiting = [From|RWait]}}.


% CAST
% @private
handle_cast(boot, S) ->
    boot(),
    {noreply, S};

handle_cast(_,S) ->
    {noreply, S}.


% INFO
% @private
handle_info({Pid, nchecks_running}, S) ->
    io:format("receive init~n"),
    case S#state.assert_init of
        undefined ->
            ok;
        F ->
            gen_server:reply(F, ok)
    end,
    erlang:link(Pid),
    Pid ! {init, {}, {empty_init}},
    {noreply,
        S#state{
            nchecks_pid      = Pid,
            replies_waiting = [],
            assert_init     = undefined
        }
    };

% @private
handle_info(stop, S) ->
    io:format("received stop~n"),
    {noreply, S};

handle_info({reply, From, Reply}, #state{replies_waiting = RWait} = S) ->
    gen_server:reply(From, Reply),
    {noreply,
        S#state{
            replies_waiting = lists:delete(From, RWait)
        }
    };

handle_info({'EXIT', Pid, Reason}, #state{nchecks_pid = Pid} = S) ->
    io:format("nchecks EXIT with reason: ~p~n", [Reason]),
    {stop, Reason, S};

handle_info(_I, S) ->
    io:format("received handle info: ~p~n", [_I]),
    {noreply, S}.


% TERMINATE
% @private
terminate(_,_) ->
    ok.


% CHANGE
% @private
code_change(_,S,_) ->
    {ok, S}.


% PRIVATE
boot() ->
    Prefix   = sysmo:get_java_bin_prefix(),
    Relative = string:concat("nchecks/bin/nchecks", Prefix),
    Cmd = filename:join(filename:absname(sysmo:get_java_dir()),Relative),
    WorkDir = filename:absname(""),
    Node = sysmo:get_node_name(),
    erlang:open_port({spawn_executable, Cmd},
                     [{args,[WorkDir, Node]}, stderr_to_stdout]).

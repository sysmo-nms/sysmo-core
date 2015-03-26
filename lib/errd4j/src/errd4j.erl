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

-module(errd4j).
-include("include/errd4j.hrl").
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


-export([
    create_test/0,
    create/3,
    create/4,
    update/3,
    graph/0,
    updates/0,
    update_fetch/0,
    updates_fetch/0,
    test/0
]).

-record(state, {
    errd4j_pid      = undefined,
    replies_waiting = [],
    java_com        = "",
    assert_init     = undefined
}).

-define(ASSERT_TIMEOUT, 5000).

update_fetch() ->
    File    = "test.rrd",
    Updates = [{"speed", 3000}],
    Fetch   = [{"speed", -600}],
    Args = {File, Updates, Fetch},
    gen_server:call(?MODULE, {call_errd4j, {update_fetch, Args}}).

updates_fetch() ->
    File    = "test.rrd",
    Updates = [{"speed", 3000}],
    Fetchs  = [{"speed", -600}],
    Args = [{File, Updates, Fetchs}],
    gen_server:call(?MODULE, {call_errd4j, {updates_fetch, Args}}).


test() ->
    gen_server:call(?MODULE, {call_errd4j, {test, {}}}).

graph() ->
    File    = "test.rrd",
    DstFile = "test.png",
    Start   = 0,
    End     = 0,
    Graphs  = [],
    Args = {File, DstFile, Start, End, Graphs},
    gen_server:call(?MODULE, {call_errd4j, {graph, Args}}).

update(File, Updates, Timestamp) ->
    %Updates = [{"MaxRoundTrip", 3000}, {"MinRoundTrip", 399}],
    Args = {File, Updates, Timestamp},
    gen_server:call(?MODULE, {call_errd4j, {update, Args}}).

updates() ->
    Updates = [{"test.rrd", [{"speed", 3000}]}],
    gen_server:call(?MODULE, {call_errd4j, {updates, Updates}}).
    
create_test() ->
    File = "test.rrd",
    Step = 300,
    DSs  = [{"speed", ?DS_COUNTER, 600, 'Nan', 'Nan'}],
    create(File,Step, DSs).
    
create(File,Step, DSs) ->

    create(File,Step,DSs, "default").

-spec create(File::string(), Step::integer(), DSs::[], RRAType::string()) -> ok.
% @doc
% where RRAType is "default" or "precise"
% @end
create(File, Step, DSs, RRAType) ->
    Args = {File,Step,RRAType,DSs},
    gen_server:call(?MODULE, {call_errd4j, {create, Args}}).

% @private
start_link() ->
    Ret = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    ok = assert_init(),
    Ret.

% @private
% @doc
% Called by start_link to ensure initialisation before returning.
% @end
assert_init() ->
    gen_server:call(?MODULE, assert_init, ?ASSERT_TIMEOUT).



% GEN_SERVER
% @private
init([]) ->
    process_flag(trap_exit, true),
    {ok, JavaConf}  = application:get_env(errd4j, java_node),
    JavaCommand     = proplists:get_value(java_com, JavaConf),
    gen_server:cast(?MODULE, boot),
    {ok, #state{java_com = JavaCommand}}.

% CALL 
% @private
handle_call(assert_init, F, #state{errd4j_pid = undefined} = S) ->
    {noreply, S#state{assert_init = F}};
handle_call(assert_init, _F, S) ->
    {reply, ok, S};

handle_call({call_errd4j, {Command, Payload}}, From, 
        #state{errd4j_pid = Errd4j, replies_waiting = RWait} = S) ->
    Errd4j ! {Command, From, Payload},
    {noreply, S#state{replies_waiting = [From|RWait]}}.


% CAST
% @private
handle_cast(boot, #state{java_com = JavaCommand} = S) ->
    boot(JavaCommand),
    {noreply, S};

handle_cast(_,S) ->
    {noreply, S}.


% INFO
% @private
handle_info({Pid, errd4j_running}, S) ->
    io:format("receive init~n"),
    case S#state.assert_init of
        undefined -> 
            ok;
        F ->
            gen_server:reply(F, ok)
    end,
    erlang:link(Pid),
    {noreply, 
        S#state{
            errd4j_pid      = Pid,
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

handle_info({'EXIT', Pid, Reason}, #state{errd4j_pid = Pid} = S) ->
    io:format("rrd4j EXIT with reason: ~p~n", [Reason]),
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
boot(JavaCommand) ->
    erlang:spawn(os,cmd,[JavaCommand]).

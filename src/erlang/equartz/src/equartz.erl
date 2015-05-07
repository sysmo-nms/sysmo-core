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

-module(equartz).
-include("include/equartz.hrl").
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
    test/0,
    register_internal_job/3,
    register_system_job/3,
    delete_job/1,
    job_exists/1,
    which_jobs/0,
    fire_now/1
]).

-record(state, {
    equartz_pid     = undefined,
    replies_waiting = [],
    assert_init     = undefined
}).

-define(ASSERT_TIMEOUT, 5000).

test() ->
    register_internal_job("jojojob", "0/20 * * * * ?", {io,format,"hello truggerrrr"}).

-spec register_internal_job(
        Name::string(),
        Trigger::string(),
        {Mod::atom(), Func::atom(), Arg::string()}
    ) -> ok.
% @doc
% Register internal job.
% Register the job with name Name triggered with rule defined in Trigger and
% launch spawn(Mod, Func, [Arg]) when fired.
% @end
register_internal_job(Name, Trigger, {M,F,A}) ->
    gen_server:call(?MODULE, {call_equartz, 
        {register_internal_job, {Name, Trigger, {M,F,A}}}}, infinity).

-spec register_system_job(
        Name::string(),
        Trigger::string(),
        Command::string()
    ) -> ok.
% @doc
% Register system job.
% Register the job with name Name triggered with rule defined in Trigger and
% launch the system command Command when fired.
% @end
register_system_job(Name, Trigger, Command) ->
    gen_server:call(?MODULE, {call_equartz, 
        {register_system_job, {Name, Trigger, Command}}}, infinity).

-spec delete_job(Name::string()) -> ok | {error, no_such_job}.
% @doc
% Delete a job.
% Delete job identified by Name if it exists.
% @end
delete_job(Name) ->
    gen_server:call(?MODULE, {call_equartz, {delete_job, {Name}}}, infinity).

-spec which_jobs() -> {ok, Jobs::[string()]}.
% @doc
% Return all registered jobs.
% @end
which_jobs() ->
    gen_server:call(?MODULE, {call_equartz, {which_jobs, {}}}, infinity).

-spec job_exists(JobName::string()) -> true | false | {error, Error::string()}.
% @doc
% Check if the job allready exists.
% @end
job_exists(JobName) ->
    gen_server:call(?MODULE, {call_equartz, {job_exists, {JobName}}}, infinity).

-spec fire_now(JobName::string()) -> ok.
% @doc
% Fire the job JobName as soon as possible.
% @end
fire_now(JobName) ->
    gen_server:call(?MODULE, {call_equartz, {fire_now, {JobName}}}, infinity).

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
handle_call(assert_init, F, #state{equartz_pid = undefined} = S) ->
    {noreply, S#state{assert_init = F}};
handle_call(assert_init, _F, S) ->
    {reply, ok, S};

handle_call({call_equartz, {Command, Payload}}, From, 
        #state{equartz_pid = NChecks, replies_waiting = RWait} = S) ->
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
handle_info({Pid, equartz_running}, S) ->
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
            equartz_pid      = Pid,
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

handle_info({fire, M,F,A}, S) ->
    spawn(M,F,[A]),
    {noreply, S};

handle_info({'EXIT', Pid, Reason}, #state{equartz_pid = Pid} = S) ->
    io:format("equartz EXIT with reason: ~p~n", [Reason]),
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
% @private
boot() ->
    Cmd = filename:join(
            filename:absname(sysmo:get_java_dir()),
            "equartz/bin/equartz"),
    Log = filename:join(
            filename:absname(sysmo:get_log_dir()),
            "equartz.log"),
    erlang:open_port({spawn_executable, Cmd}, [{args,[Log]}, stderr_to_stdout]).

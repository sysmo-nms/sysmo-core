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

-module(monitor_scheduler).
-include("include/monitor.hrl").
-include_lib("common_hrl/include/logs.hrl").
-behaviour(gen_server).

% gen_server
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,
         terminate/2,code_change/3]).

-export([start_link/0]).

% API
-export([test/0,register_job/3,delete_job/1,job_exists/1,
         which_jobs/0,fire_now/1]).



% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



-spec register_job(
        Job::string(),
        Trigger::string(),
        {Mod::atom(), Func::atom(), Args::string()}
    ) -> ok.
% @doc
% Register internal job.
% Register the job with name Job triggered with rule defined in Trigger and
% launch spawn(Mod, Func, Args) when fired.
% @end
register_job(Job, Step, MFA) ->
    gen_server:call(?MODULE,
                    {register_job, Job, Step, MFA}).



-spec delete_job(Job::string()) -> ok | no_such_job.
% @doc
% Delete a job.
% Delete job identified by Job if it exists.
% @end
delete_job(Job) ->
    gen_server:call(?MODULE, {delete_job, Job}).



-spec which_jobs() -> {ok, Jobs::[string()]}.
% @doc
% Return all registered jobs.
% @end
which_jobs() ->
    gen_server:call(?MODULE, which_jobs).



-spec job_exists(Job::string()) -> true | false.
% @doc
% Check if the job allready exists.
% @end
job_exists(Job) ->
    gen_server:call(?MODULE, {job_exists, Job}).



-spec fire_now(Job::string()) -> ok.
% @doc
% Fire the job Job as soon as possible.
% @end
fire_now(Job) ->
    gen_server:call(?MODULE, {fire_now, Job}).



% @private
init([]) ->
    {ok, []}.



% @private
handle_call({register_job, Job, Step, MFA}, _From, JobList) ->
    InitialStep = random:uniform(Step),
    TRef = erlang:send_after(InitialStep, self(), {fire, Job, Step, MFA}),
    {reply, ok, [{Job, Step, MFA, TRef} | JobList]};

handle_call({delete_job, Job}, _From, JobList) ->
    case lists:keytake(Job, 1, JobList) of
        false ->
            {reply, no_such_job, JobList};
        {value, {_,_,_,TRef}, JobList2} ->
            erlang:cancel_timer(TRef),
            {reply, ok, JobList2}
    end;

handle_call({job_exists, Job}, _From, JobList) ->
    case lists:keyfind(Job, 1, JobList) of
        false -> {reply, false, JobList};
        _     -> {reply, true,  JobList}
    end;

handle_call(which_jobs, _From, JobList) ->
    Jobs = [J || {J,_,_,_} <- JobList],
    {reply, {ok, Jobs}, JobList};

handle_call({fire_now, Job}, _From, JobList) ->
    case lists:keyfind(Job, 1, JobList) of
        false -> {reply, no_shuch_job, JobList};
        {Job, Step, {M,F,A}, OldTRef} ->
            erlang:cancel_timer(OldTRef),
            spawn(M,F,[A]),
            TRef = erlang:send_after(Step, self(), {fire, Job, Step, {M,F,A}}),
            NewJobList = lists:keyreplace(Job, 1, JobList,
                                          {Job, Step, {M,F,A}, TRef}),
            {reply, ok, NewJobList}
    end;

handle_call(Call, _, S) ->
    ?LOG_WARNING("Received unknow handle cast: ", Call),
    {noreply, S}.


% @private
handle_info({fire, Job, Step, {M,F,A}}, JobList) ->
    ?LOG_INFO("Job fired", Job),
    spawn(M, F, [A]),
    TRef = erlang:send_after(Step, self(), {fire, Job, Step, {M,F,A}}),
    NewJobList = lists:keyreplace(Job, 1, JobList, {Job, Step, {M,F,A}, TRef}),
    {noreply, NewJobList};

handle_info(I, S) ->
    ?LOG_WARNING("Received handle info:", I),
    {noreply, S}.



% @private
handle_cast(Cast,S) ->
    ?LOG_WARNING("Received unknow handle cast: ", Cast),
    {noreply, S}.

% @private
terminate(_,_) ->
    ?LOG_INFO("Received terminate"),
    ok.

% @private
code_change(_,S,_) ->
    {ok, S}.

test() ->
    Step = 5000, % every 5 seconds
    register_job("jojojob", Step , {io,format, "hello trigggger"}).

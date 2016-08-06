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

-module(monitor_scheduler).
-include("monitor.hrl").
-include_lib("common_hrl/include/logs.hrl").
-behaviour(gen_server).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-export([start_link/0]).

% API
-export([test/0, register_job/3, delete_job/1, job_exists/1,
         which_jobs/0, fire_now/1]).

-export([clean_sync_dir/1]).

% @private
start_link() ->
    Ret = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    % TODO should be a supercast work
    % clean sync dir every hours
    ok = register_job("clean_sync_dir", 3600000, {?MODULE, clean_sync_dir, ""}),
    Ret.



-spec register_job(
        Job::string(),
        Trigger::integer(),
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

handle_call(_Call, _, S) ->
    ?LOG_WARNING("Received unknow handle cast: ", _Call),
    {noreply, S}.


% @private
handle_info({fire, Job, Step, {M,F,A}}, JobList) ->
    ?LOG_INFO("Job fired", Job),
    spawn(M, F, [A]),
    TRef = erlang:send_after(Step, self(), {fire, Job, Step, {M,F,A}}),
    NewJobList = lists:keyreplace(Job, 1, JobList, {Job, Step, {M,F,A}, TRef}),
    {noreply, NewJobList};

handle_info(_I, S) ->
    ?LOG_WARNING("Received handle info:", _I),
    {noreply, S}.



% @private
handle_cast(_Cast,S) ->
    ?LOG_WARNING("Received unknow handle cast: ", _Cast),
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


% Clean sync dir
clean_sync_dir(_) ->
    {ok, DumpDir} = application:get_env(monitor,http_sync_dir),
    case file:list_dir(DumpDir) of
        {ok, []} -> ok;
        {ok, Files} ->
            clean_sync_dir(DumpDir, Files);
        _ -> ok
    end.
clean_sync_dir(_,[]) -> ok;
clean_sync_dir(DumpDir, [H|T]) ->
    % delete all directory older than five minutes
    Dir   = filename:join(DumpDir,H),
    Date   = filelib:last_modified(Dir),
    Local  = calendar:local_time(),
    Date2  = calendar:datetime_to_gregorian_seconds(Date),
    Local2 = calendar:datetime_to_gregorian_seconds(Local),
    Diff = Local2 - Date2,
    case (Diff > 300) of
        true ->
            del_dir(Dir),
            clean_sync_dir(DumpDir,T);
        false ->
            clean_sync_dir(DumpDir,T)
    end.

del_dir(Dir) ->
   lists:foreach(fun(D) ->
                    ok = file:del_dir(D)
                 end, del_all_files([Dir], [])).

del_all_files([], EmptyDirs) ->
   EmptyDirs;
del_all_files([Dir | T], EmptyDirs) ->
   {ok, FilesInDir} = file:list_dir(Dir),
   {Files, Dirs} = lists:foldl(fun(F, {Fs, Ds}) ->
                                  Path = Dir ++ "/" ++ F,
                                  case filelib:is_dir(Path) of
                                     true ->
                                          {Fs, [Path | Ds]};
                                     false ->
                                          {[Path | Fs], Ds}
                                  end
                               end, {[],[]}, FilesInDir),
   lists:foreach(fun(F) ->
                         ok = file:delete(F)
                 end, Files),
   del_all_files(T ++ Dirs, [Dir | EmptyDirs]).

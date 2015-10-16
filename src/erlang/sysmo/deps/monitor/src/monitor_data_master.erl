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
-module(monitor_data_master).
-behaviour(gen_server).
-include("monitor.hrl").
-include_lib("common_hrl/include/logs.hrl").

% GEN_SERVER
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-export([start_link/0]).

% API
-export([new/2, get/2, update/2, delete/2, iterate/2, get_jobs/1,
    get_probes/1, which/1]).

-export([get_probe_state/1, set_probe_state/1, del_probe_state/1]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------
-spec which(Table::target|probe|job) -> [string()].
% @doc
% Return all keys of the table specified.
% @end
which(Table) ->
    gen_server:call(?MODULE, {which, Table}).

do_which(target) -> mnesia:dirty_all_keys(target);
do_which(probe)  -> mnesia:dirty_all_keys(probe);
do_which(job)    -> mnesia:dirty_all_keys(job);
do_which(dependency) -> mnesia:dirty_all_keys(dependency);
do_which(_)      -> error.


-spec new(Table::target|probe|job|dependency, Record::#target{}|#probe{}|#job{}) -> Name::string().
% @doc
% Initialize the new element and add it to the table. It include giving him a
% name, and call the appropriate API to initialize it.
% @end
new(Table, Record) ->
    gen_server:call(?MODULE, {new, Table, Record}).

do_new(job, Job) ->
    Name = generate_id(job),
    IJob = Job#job{name=Name},
    mnesia:dirty_write(IJob),
    #job{trigger=Tr,module=M,function=F,argument=A} = IJob,
    monitor_scheduler:register_job(Name,Tr,{M,F,A}),
    Name;
do_new(probe, Probe) ->
    Name = generate_id(probe),
    IProbe = Probe#probe{name=Name},
    mnesia:dirty_write(IProbe),
    launch_probe(IProbe),
    Name;
do_new(target, Target) ->
    {ok, DataDir} = application:get_env(monitor,targets_data_dir),
    Name = generate_id(target),
    VDir = {var_directory, filename:join(DataDir,Name)},
    SP = lists:keystore(var_directory, 1, Target#target.sys_properties, VDir),
    SName = {"staticName", Name},
    P = lists:keystore("staticName", 1, Target#target.properties, SName),
    ITarget = Target#target{name=Name,sys_properties=SP,properties=P},

    % if register target snmp (v3) is valid (USM user valid) continue
    case monitor_utils:init_target_snmp(ITarget) of
        ok ->
            monitor_utils:init_target_dir(ITarget),
            mnesia:dirty_write(ITarget),
            Name;
        Error ->
            Error
    end;
do_new(dependency, Dep) ->
    mnesia:dirty_write(Dep),
    Dep#dependency.a_probe.



-spec update(Table::target|probe|job|dependency, Record::#target{}|#probe{}|#job{}) -> 
    ok | abort | {error, Reason::string}.
% @doc
% Overwrite an element in the table. Fail if the element did not exist.
% @end
update(Table, Record) ->
    gen_server:call(?MODULE, {update, Table, Record}).

do_update(target, #target{name=Key} = Target) ->
    case mnesia:dirty_read(target, Key) of
        []  -> {error, "Unknown target"};
        [_] -> mnesia:dirty_write(Target)
    end;
do_update(probe, #probe{name=Key} = Probe) ->
    case mnesia:dirty_read(probe, Key) of
        []  -> {error, "Unknown probe"};
        [_] -> mnesia:dirty_write(Probe)
    end;
do_update(job, #job{name=Key} = Job) ->
    case mnesia:dirty_read(job, Key) of
        []  -> {error, "Unknown job"};
        [_] -> mnesia:dirty_write(Job)
    end;
do_update(dependency, Dep) ->
    mnesia:dirty_write(Dep),
    Dep#dependency.a_probe.


-spec delete(Table::target|probe|job|dependency, Key::string()) -> ok | abort.
% @doc
% Delete the specified element from the table. Deleting a target will also
% delete his probes and jobs.
% @end
delete(Table, Key) ->
    gen_server:call(?MODULE, {delete, Table, Key}).

do_delete(target, Key) ->
    lists:foreach(fun(J) -> do_delete(job,   J) end, do_get_jobs(Key)),
    lists:foreach(fun(P) -> do_delete(probe, P) end, do_get_probes(Key)),
    monitor_utils:cleanup_target_snmp(Key),
    mnesia:dirty_delete({target,Key});
do_delete(probe, Key) ->
    shutdown_probe(Key),
    case do_get(dependency, Key) of
        []  -> ok;
        [_] ->
            do_delete(dependency, Key),
            % delete ciblings
            Others = mnesia:dirty_select(dependency,
                [
                    {#dependency{a_probe='$1',his_parent=Key,_='_'},
                    [],
                    ['$1']}
                ]
            ),
            lists:foreach(fun(X) -> do_delete(dependency, X) end, Others)
    end,
    mnesia:dirty_delete({probe,Key});
do_delete(job, Key) ->
    monitor_scheduler:delete_job(Key),
    mnesia:dirty_delete({job, Key});
do_delete(dependency, Key) ->
    mnesia:dirty_delete({dependency, Key}).


-spec iterate(Table::target|probe|job|dependency, Fun::fun()) -> ok.
% @doc
% Iterate a table. Fun must accept two arguments:
% - a element record,
% - an accumulator with default value [].
% @end
iterate(Table, Fun) ->
    gen_server:call(?MODULE, {iterate, Table, Fun}).

do_iterate(Table, Fun) ->
    do_iterate(Table, mnesia:dirty_first(Table), Fun, []).
do_iterate(_, '$end_of_table', _, Acc) -> {ok, Acc};
do_iterate(Tab, Key, Fun, Acc) ->
    [Element] = mnesia:dirty_read(Tab, Key),
    Acc2 = Fun(Element,Acc),
    do_iterate(Tab,mnesia:dirty_next(Tab, Key),Fun,Acc2).


-spec get(Table::target|probe|job|dependency, Key::string()) -> #target{} | #probe{} | #job{} | undefined.
% @doc
% Get an element by key. Return the table record corresponding to the key Key.
% @end
get(Table, Key) ->
    gen_server:call(?MODULE, {get, Table, Key}).

do_get(Table, Key) ->
    mnesia:dirty_read(Table, Key).



-spec get_jobs(Key::string()) -> [string()].
% @doc
% Return all jobs which belong to target identified by Key.
% @end
get_jobs(Key) ->
    gen_server:call(?MODULE, {get_jobs, Key}).

do_get_jobs(Key) ->
    mnesia:dirty_select(job,[{#job{name='$1',belong_to=Key,_='_'},[],['$1']}]).


-spec get_probes(TargetKey::string()) -> [string()].
% @doc
% Return all probes which belong to target TargetKey.
% @end
get_probes(Key) ->
    gen_server:call(?MODULE, {get_probes, Key}).

do_get_probes(Key) ->
    mnesia:dirty_select(probe,[{#probe{name='$1',belong_to=Key,_='_'},[],['$1']}]).


get_probe_state(Key) ->
    case ets:lookup(?PROBES_STATE, Key) of [] -> undefined; [V] -> V end.
set_probe_state(State) ->
    ets:insert(?PROBES_STATE, State).
del_probe_state(Key) ->
    ets:delete(?PROBES_STATE, Key).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%----------------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%----------------------------------------------------------------------------
init([]) ->
    init_ets_tables(),
    init_mnesia_tables(),
    init_targets(),
    init_probes(),
    init_jobs(),
    {ok, nostate}.

handle_call({update, Table, Record}, _From, S) ->
    {reply, do_update(Table, Record), S};

handle_call({new, Table, Record}, _From, S) ->
    {reply, do_new(Table, Record), S};

handle_call({delete, Table, Key}, _From, S) ->
    {reply, do_delete(Table, Key), S};

handle_call({iterate, Table, Fun}, _From, S) ->
    {reply, do_iterate(Table, Fun), S};

handle_call({get, Table, Key}, _From, S) ->
    {reply, do_get(Table, Key), S};

handle_call({get_jobs, Key}, _From, S) ->
    {reply, do_get_jobs(Key), S};

handle_call({get_probes, Key}, _From, S) ->
    {reply, do_get_probes(Key), S};

handle_call({which, Table}, _From, S) ->
    {reply, do_which(Table), S};

handle_call(_Call, _From, S) ->
    {noreply, S}.


handle_cast(_R, S) ->
    {noreply, S}.

handle_info(_I, S) ->
    {noreply, S}.

terminate(_R, _S) ->
    normal.

code_change(_O, S, _E) ->
    {ok, S}.

%%----------------------------------------------------------------------------
%% local functions
%%----------------------------------------------------------------------------
init_ets_tables() ->
    ets:new(?PROBES_STATE,
        [
            set,
            named_table,
            public,
            compressed,
            {keypos, 2}
        ]
    ).

init_mnesia_tables() ->
    Tables = mnesia:system_info(tables),
    DetsOpts = [
        {auto_save, 5000}
    ],
    case lists:member(target, Tables) of
        true -> ok;
        false ->
            {atomic,ok} = mnesia:create_table(
                target,
                [
                    {attributes, record_info(fields, target)},
                    {disc_copies, [node()]},
                    {storage_properties, [{dets, DetsOpts}]}
                ]
            )
    end,
    case lists:member(probe, Tables) of
        true -> ok;
        false ->
            {atomic,ok} = mnesia:create_table(
                probe,
                [
                    {attributes, record_info(fields, probe)},
                    {disc_copies, [node()]},
                    {index, [belong_to]},
                    {storage_properties, [{dets, DetsOpts}]}
                ]
            )
    end,
    case lists:member(job, Tables) of
        true -> ok;
        false ->
            {atomic,ok} = mnesia:create_table(
                job,
                [
                    {attributes, record_info(fields, job)},
                    {disc_copies, [node()]},
                    {index, [belong_to]},
                    {storage_properties, [{dets, DetsOpts}]}
                ]
            )
    end,
    case lists:member(dependency, Tables) of
        true -> ok;
        false ->
            {atomic,ok} = mnesia:create_table(
                dependency,
                [
                    {attributes, record_info(fields, dependency)},
                    {disc_copies, [node()]},
                    {index, [his_parent]},
                    {storage_properties, [{dets, DetsOpts}]}
                ]
            )
    end,
    mnesia:wait_for_tables([target, job, probe, dependency], 2000).


init_targets() ->
    do_iterate(target, fun(T,_) ->
        monitor_utils:init_target_snmp(T),
        monitor_utils:init_target_dir(T)
    end).

init_probes() ->
    do_iterate(probe, fun(P,_) ->
        launch_probe(P)
    end).

init_jobs() ->
    do_iterate(job, fun(J,_) ->
        #job{name=Name,trigger=Tr,module=M,function=F,argument=A} = J,
        monitor_scheduler:register_job(Name,Tr,{M,F,A})
    end).

launch_probe(#probe{module=nchecks_probe} = Probe) ->
    nchecks_probe_sup:launch(Probe).

%%----------------------------------------------------------------------------
%% UTILS
%%----------------------------------------------------------------------------
generate_id(Table) ->
    Id = lists:concat([Table, [$-|generate_id()]]),
    case mnesia:dirty_read(Table, Id) of
        []  -> Id;
        [_] -> generate_id(Table)
    end.
generate_id() ->
    B = crypto:rand_bytes(8),
    N = lists:foldl(fun(N,Acc) -> Acc * 256 + N end, 0, binary_to_list(B)),
    integer_to_list(N).

shutdown_probe(PidName) ->
    case supercast_registrar:whereis_name(PidName) of
        undefined -> ok;
        Pid ->
            gen_server:call(Pid, shut_it_down)
    end.

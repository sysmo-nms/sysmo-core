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
-include("include/monitor.hrl").

% GEN_SERVER
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
    new/2,
    get/2,
    update/2,
    delete/2,
    iterate/2,

    % probe state
    get_probe_state/1,
    set_probe_state/1,
    del_probe_state/1
]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------
-spec new(Table::target|probe|job, Record::#target{}|#probe{}|#job{}) -> Name::string().
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
    equartz:register_internal_job(Name,Tr,{M,F,A}),
    Name;
do_new(probe, Probe) ->
    Name = generate_id(probe),
    IProbe = Probe#probe{name=Name},
    mnesia:dirty_write(IProbe),
    monitor_probe_sup:launch(IProbe),
    Name;
do_new(target, Target) ->
    {ok, DataDir} = application:get_env(monitor,targets_data_dir),
    Name = generate_id(target),
    VDir = {var_directory, filename:join(DataDir,Name)},
    SP   = lists:keystore(var_directory, 1, Target#target.sys_properties, VDir),
    SName = {"staticName", Name},
    P    = lists:keystore("staticName", 1, Target#target.properties, SName),
    ITarget = Target#target{name=Name,sys_properties=SP,properties=P},
    mnesia:dirty_write(ITarget),
    monitor_utils:init_target_snmp(ITarget),
    monitor_utils:init_target_dir(ITarget),
    Name.


-spec update(Table::target|probe|job, Record::#target{}|#probe{}|#job{}) -> 
    ok | abort | {error, Reason::string}.
% @doc
% Overwrite an element in the table. Fail if the element did not exist.
% @end
update(Table, Record) ->
    gen_server:call(?MODULE, {update, Table, Record}).

do_update(target, #target{name=Key} = Target) ->
    case mnesia:dirty_read(target, Key) of
        []  -> {error, "Unknown target"};
        [_] -> mnesia:dirty_write(Target), Key
    end;
do_update(probe, #probe{name=Key} = Probe) ->
    case mnesia:dirty_read(probe, Key) of
        []  -> {error, "Unknown probe"};
        [_] -> mnesia:dirty_write(Probe), Key
    end;
do_update(job, #job{name=Key} = Job) ->
    case mnesia:dirty_read(job, Key) of
        []  -> {error, "Unknown job"};
        [_] -> mnesia:dirty_write(Job), Key
    end.


-spec delete(Table::target|probe|job, Key::string()) -> ok | abort.
% @doc
% Delete the specified element from the table. Deleting a target will also
% delete his probes and jobs.
% @end
delete(Table, Key) ->
    gen_server:call(?MODULE, {delete, Table, Key}).

do_delete(target, Key) ->
    mnesia:dirty_delete({target,Key});
do_delete(Table, Key) ->
    mnesia:dirty_delete({Table, Key}).


-spec iterate(Table::target|probe|job, Fun::fun()) -> ok.
% @doc
% Iterate a table. To have a description of Fun, see mnesia:foldl/3.
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


-spec get(Table::target|probe|job, Key::string()) -> #target{} | #probe{} | #job{} | undefined.
% @doc
% Get an element by key. Return the table record corresponding to the key Key.
% @end
get(Table, Key) ->
    gen_server:call(?MODULE, {get, Table, Key}).

do_get(Table, Key) ->
    case mnesia:dirty_read(Table, Key) of
        []  -> undefined;
        [V] -> V
    end.


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

handle_call({new, Table, Record}, _From, S) ->
    {reply, do_new(Table, Record), S};

handle_call({update, Table, Record}, _From, S) ->
    {reply, do_update(Table, Record), S};

handle_call({delete, Table, Key}, _From, S) ->
    {reply, do_delete(Table, Key), S};

handle_call({iterate, Table, Fun}, _From, S) ->
    {reply, do_iterate(Table, Fun), S};

handle_call({get, Table, Key}, _From, S) ->
    {reply, do_get(Table, Key), S};

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
    end.

init_targets() ->
    do_iterate(target, fun(T,_) ->
        monitor_utils:init_target_snmp(T),
        monitor_utils:init_target_dir(T)
    end).

init_probes() ->
    do_iterate(probe, fun(P,_) ->
        monitor_probe_sup:launch(P)
    end).

init_jobs() ->
    do_iterate(job, fun(J,_) ->
        #job{name=Name,trigger=Tr,module=M,function=F,argument=A} = J,
        equartz:register_internal_job(Name,Tr,{M,F,A})
    end).

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

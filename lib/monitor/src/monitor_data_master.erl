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
new(job, Job) ->
    Trans = fun() ->
        Name = generate_id(job),
        J1 = Job#job{name=Name},
        mnesia:write(J1),
        J1
    end,
    {atomic, IJob} = mnesia:transaction(Trans),
    #job{name=Name,trigger=Tr,module=M,function=F,argument=A} = IJob,
    equartz:register_internal_job(Name,Tr,{M,F,A}),
    Name;
new(probe, Probe) ->
    Trans = fun() ->
        Name = generate_id(probe),
        P2 = Probe#probe{name=Name},
        mnesia:write(P2),
        P2
    end,
    {atomic, IProbe} = mnesia:transaction(Trans),
    monitor_probe_sup:launch(IProbe),
    IProbe#probe.name;
new(target, Target) ->
    {ok, DataDir} = application:get_env(monitor,targets_data_dir),
    Trans = fun() ->
        Name = generate_id(target),
        VDir = {var_directory, filename:join(DataDir,Name)},
        SP   = lists:keystore(var_directory, 1, Target#target.sys_properties, VDir),

        SName = {"staticName", Name},
        P  = lists:keystore("staticName", 1, Target#target.properties, SName),

        T2 = Target#target{
            name            = Name,
            sys_properties  = SP,
            properties      = P
        },
        mnesia:write(T2),
        T2
    end,
    {atomic, ITarget} = mnesia:transaction(Trans),
    monitor_utils:init_target_snmp(ITarget),
    monitor_utils:init_target_dir(ITarget),
    ITarget#target.name.

-spec update(Table::target|probe|job, Record::#target{}|#probe{}|#job{}) -> 
    ok | abort | {error, Reason::string}.
% @doc
% Overwrite an element in the table. Fail if the element did not exist.
% @end
update(target, #target{name=Key} = Target) ->
    Trans = fun() ->
        case mnesia:read({target, Key}) of
            []  -> {error, "Unknown target"};
            [_] -> mnesia:write(Target), Key
        end
    end,
    {atomic, R} = mnesia:transaction(Trans),
    R;
update(probe, #probe{name=Key} = Probe) ->
    Trans = fun() ->
        case mnesia:read({probe, Key}) of
            []  -> {error, "Unknown probe"};
            [_] -> mnesia:write(Probe), Key
        end
    end,
    {atomic, R} = mnesia:transaction(Trans),
    R;
update(job, #job{name=Key} = Job) ->
    Trans = fun() ->
        case mnesia:read({job, Key}) of
            []  -> {error, "Unknown job"};
            [_] -> mnesia:write(Job), Key
        end
    end,
    {atomic, R} = mnesia:transaction(Trans),
    R.

-spec delete(Table::target|probe|job, Key::string()) -> ok | abort.
% @doc
% Delete the specified element from the table.
% @end
delete(Table, Key) ->
    {atomic, R} = mnesia:transaction(
        fun() -> mnesia:delete({Table, Key}) end
    ), R.




-spec iterate(Table::target|probe|job, Fun::fun()) -> ok.
% @doc
% Iterate a table. To have a description of Fun, see mnesia:foldl/3.
% @end
iterate(Table, Fun) ->
    Trans = fun() -> mnesia:foldl(Fun, [], Table) end,
    mnesia:transaction(Trans).

-spec get(Table::target|probe|job, Key::string()) -> #target{} | #probe{} | #job{} | undefined.
% @doc
% Get an element by key. Return the table record corresponding to the key Key.
% @end
get(Table, Key) ->
    {atomic, T} =  mnesia:transaction(fun() -> 
        case mnesia:read({Table, Key}) of
            []  -> undefined;
            [V] -> V
        end
    end),
    T.

% @private
get_probe_state(Key) ->
    case ets:lookup(?PROBES_STATE, Key) of
        [] -> undefined;
        [V] -> V
    end.

% @private
set_probe_state(State) ->
    ets:insert(?PROBES_STATE, State).

% @private
del_probe_state(Key) ->
    ets:delete(?PROBES_STATE, Key).

% @private
generate_id(Table) ->
    Id = lists:concat([Table, [$-|generate_id()]]),
    case mnesia:read({Table, Id}) of
        []  -> Id;
        [_] -> generate_id(Table)
    end.



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
    {ok, state}.

handle_call(_Call, _From, S) ->
    {noreply, S}.

handle_cast(_R, S) ->
    {noreply, S}.

handle_info(_I, S) ->
    ?LOG({"handle info: ", _I}),
    {noreply, S}.

terminate(_R, state) ->
    normal.

code_change(_O, S, _E) ->
    {ok, S}.


%%----------------------------------------------------------------------------
%% local functions
%%----------------------------------------------------------------------------
% MNESIA init
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
                    {storage_properties,
                        [
                            {dets, DetsOpts}
                        ]
                    }

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
                    {storage_properties,
                        [
                            {dets, DetsOpts}
                        ]
                    }
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
                    {index, [belong_to]},
                    {disc_copies, [node()]},
                    {storage_properties,
                        [
                            {dets, DetsOpts}
                        ]
                    }
                ]
            )
    end.

% ETS probes_states init
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

%%----------------------------------------------------------------------------
%% UTILS    
%%----------------------------------------------------------------------------
init_targets() ->
    {atomic, R} = iterate(target, fun(T,_) ->
        monitor_utils:init_target_snmp(T),
        monitor_utils:init_target_dir(T)
    end),
    {ok, R}.

init_probes() ->
    {atomic, R} = iterate(probe, fun(P,_) ->
        monitor_probe_sup:launch(P)
    end),
    {ok, R}.

init_jobs() ->
    {atomic, R} = iterate(job, fun(J,_) ->
        #job{name=Name,trigger=Tr,module=M,function=F,argument=A} = J,
        equartz:register_internal_job(Name,Tr,{M,F,A})
    end),
    {ok, R}.


generate_id() ->
    B = crypto:rand_bytes(8),
    N = lists:foldl(fun(N,Acc) -> Acc * 256 + N end, 0, binary_to_list(B)),
    integer_to_list(N).

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
-module(monitor_data).
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
    write_target/1,
    write_probe/1,
    write_job/1,

    iterate_target_table/1,
    iterate_probe_table/1,
    iterate_job_table/1,

    get_target/1,
    get_probe/1,
    get_job/1,

    del_target/1,

    get_probe_state/1,
    set_probe_state/1,
    del_probe_state/1,

    generate_id/1
]).


%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------
% MNESIA write
write_target(#target{name=undefined} = Target) ->
    Trans = fun() ->
        {ok, DataDir} = application:get_env(monitor,targets_data_dir),
        Name = generate_id(target),

        VDir = {var_directory, filename:join(DataDir,Name)},
        SP = lists:keystore(var_directory, 1, Target#target.sys_properties, VDir),

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
    mnesia:transaction(Trans);
write_target(Target) ->
    mnesia:transaction(fun() -> mnesia:write(Target), Target end).

write_probe(#probe{name=undefined} = Probe)   ->
    Trans = fun() ->
        Name = generate_id(probe),
        P2 = Probe#probe{name=Name},
        mnesia:write(P2),
        P2
    end,
    mnesia:transaction(Trans);
write_probe(Probe)   ->
    mnesia:transaction(fun() -> mnesia:write(Probe), Probe end).

write_job(#job{name=undefined} = Job)   ->
    Trans = fun() ->
        Name = generate_id(job),
        J1 = Job#job{name=Name},
        mnesia:write(J1),
        J1
    end,
    mnesia:transaction(Trans);
write_job(Job)       ->
    mnesia:transaction(fun() -> mnesia:write(Job), Job end).


% MNESIA iterate
iterate_target_table(Fun) ->
    Trans = fun() -> mnesia:foldl(Fun, [], target) end,
    mnesia:transaction(Trans).

iterate_probe_table(Fun) ->
    Trans = fun() -> mnesia:foldl(Fun, [], probe) end,
    mnesia:transaction(Trans).

iterate_job_table(Fun) ->
    Trans = fun() -> mnesia:foldl(Fun, [], job) end,
    mnesia:transaction(Trans).

% MNESIA get
get_target(Key) ->
    {atomic, T} =  mnesia:transaction(fun() -> 
        case mnesia:read({target, Key}) of
            []  -> undefined;
            [V] -> V
        end
    end),
    T.

get_probe(Key) ->
    {atomic, P} = mnesia:transaction(fun() -> 
        case mnesia:read({probe, Key}) of
            [] -> undefined;
            [V] -> V
        end
    end),
    P.

get_job(Key) ->
    {atomic, J} = mnesia:transaction(fun() -> 
        case mnesia:read({job, Key}) of
            [] -> undefined;
            [V] -> V
        end
    end),
    J.


del_target(Key) ->
    {atomic, R} = mnesia:transaction(fun() ->
        mnesia:delete({target, Key})
    end),
    R.


get_probe_state(Key) ->
    case ets:lookup(?PROBES_STATE, Key) of
        [] -> undefined;
        [V] -> V
    end.

set_probe_state(State) ->
    ets:insert(?PROBES_STATE, State).

del_probe_state(Key) ->
    ets:delete(?PROBES_STATE, Key).

% generate_id must be used from within a mnesia:transaction
generate_id(target) ->
    Id = lists:concat([target, [$-|generate_id()]]),
    case mnesia:read({target, Id}) of
        []  -> Id;
        [_] -> generate_id(target)
    end;

generate_id(probe) ->
    Id = lists:concat([probe, [$-|generate_id()]]),
    case mnesia:read({probe, Id}) of
        []  -> Id;
        [_] -> generate_id(probe)
    end;

generate_id(job) ->
    Id = lists:concat([job, [$-|generate_id()]]),
    case mnesia:read({job, Id}) of
        []  -> Id;
        [_] -> generate_id(job)
    end.

generate_id() ->
    B = crypto:rand_bytes(8),
    N = lists:foldl(fun(N,Acc) -> Acc * 256 + N end, 0, binary_to_list(B)),
    integer_to_list(N).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



%%----------------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%----------------------------------------------------------------------------
init([]) ->
    init_ets_tables(),
    init_mnesia_tables(),
    mnesia:subscribe({table, target, detailed}),
    mnesia:subscribe({table, probe,  detailed}),
    mnesia:subscribe({table, job,    detailed}),
    {ok, state}.

handle_call(_Call, _From, S) ->
    {noreply, S}.

handle_cast(_R, S) ->
    {noreply, S}.


handle_info({mnesia_table_event, {write, target, Target, [], _ActivityId}}, S) ->
    handle_target_create(Target),
    {noreply, S};
handle_info({mnesia_table_event, {write, target, Target, [Target], _ActivityId}}, S) ->
    % same thing do nothing
    {noreply, S};
handle_info({mnesia_table_event, {write, target, NewTarget, OldTarget, _ActivityId}}, S) ->
    handle_target_update(NewTarget, OldTarget),
    {noreply, S};

handle_info({mnesia_table_event, {write, probe, Probe, [], _ActivityId}}, S) ->
    handle_probe_create(Probe),
    {noreply, S};
handle_info({mnesia_table_event, {write, probe, Probe, [Probe], _ActivityId}}, S) ->
    % same thing do nothing
    {noreply, S};
handle_info({mnesia_table_event, {write, probe, NewProbe, [OldProbe], _ActivityId}}, S) ->
    handle_probe_update(NewProbe, OldProbe),
    {noreply, S};

handle_info({mnesia_table_event, {write, job, Job, [], _ActivityId}}, S) ->
    handle_job_create(Job),
    {noreply, S};
handle_info({mnesia_table_event, {write, job, Job, [Job], _ActivityId}}, S) ->
    % same thing do nothing
    {noreply, S};
handle_info({mnesia_table_event, {write, job, NewJob, [OldJob], _ActivityId}}, S) ->
    handle_job_update(NewJob, OldJob),
    {noreply, S};

handle_info({mnesia_table_event, {delete, Table, What, _OldRecords, _ActivityId}}, S) ->
    ?LOG({"handle_info delete ", Table, What, _OldRecords}),
    {noreply, S};


handle_info(_I, S) ->
    ?LOG({"handle info: ", _I}),
    {noreply, S}.

terminate(_R, state) ->
    normal.

code_change(_O, S, _E) ->
    {ok, S}.

% MNESIA events
handle_target_create(#target{permissions=Perm} = Target) ->
    Pdu = monitor_pdu:'PDU-MonitorPDU-fromServer-infoTarget-create'(Target),
    supercast_channel:emit(?MASTER_CHANNEL, {Perm, Pdu}).

handle_target_update(#target{permissions=Perm} = Target, _) ->
    Pdu = monitor_pdu:'PDU-MonitorPDU-fromServer-infoTarget-update'(Target),
    supercast_channel:emit(?MASTER_CHANNEL, {Perm, Pdu}).


handle_probe_create(#probe{permissions=Perm} = Probe) ->
    Pdu = monitor_pdu:'PDU-MonitorPDU-fromServer-infoProbe-create'(Probe),
    supercast_channel:emit(?MASTER_CHANNEL, {Perm, Pdu}).

handle_probe_update(#probe{permissions=Perm} = Probe,_) ->
    Pdu = monitor_pdu:'PDU-MonitorPDU-fromServer-infoProbe-update'(Probe),
    supercast_channel:emit(?MASTER_CHANNEL, {Perm, Pdu}).

handle_job_create(#job{permissions=_Perm} = Job) ->
    ?LOG({create_job, Job}).
    
handle_job_update(#job{permissions=_Perm} = Job, _) ->
    ?LOG({update_job, Job}).


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

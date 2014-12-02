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
-module(monitor_master).
-behaviour(gen_server).
-behaviour(supercast_channel).
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

% GEN_CHANNEL
-export([
    get_perms/1,
    sync_request/2
]).

% SRV
-export([
    start_link/0
]).

% API
-export([
    create_target/1,
    create_probe/1,

    init_target/1
]).

-record(state, {
    perm
}).



%%----------------------------------------------------------------------------
%% monitor API
%%----------------------------------------------------------------------------
-spec create_target(Target::#target{}) -> ok | {error, Info::string()}.
% @doc
% Called from monitor_commander.
% @end
create_target(Target) ->
    gen_server:call({via, supercast_registrar, ?MASTER_CHANNEL}, {create_target, Target}).

-spec create_probe(Probe::#probe{}) 
    -> ok | {error, string()}.
% @doc
% Called from monitor_commander.
% @end
create_probe(Probe) ->
    gen_server:call({via, supercast_registrar, ?MASTER_CHANNEL}, {create_probe, Probe}).


%%----------------------------------------------------------------------------
%% supercast_channel API
%%----------------------------------------------------------------------------
-spec get_perms(PidName::atom()) -> {ok, PermConf::#perm_conf{}}.
get_perms(PidName) ->
    gen_server:call({via, supercast_registrar, PidName}, get_perms).

-spec sync_request(PidName::atom(), CState::tuple()) ->  ok.
sync_request(PidName, CState) ->
    gen_server:cast({via, supercast_registrar, PidName}, {sync_request, CState}).



start_link() ->
    gen_server:start_link(
      {via, supercast_registrar, {?MODULE, ?MASTER_CHANNEL}}, ?MODULE, [], []).

%%----------------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%----------------------------------------------------------------------------
init([]) ->
    random:seed(erlang:now()),
    {ok, Read}  = application:get_env(monitor, master_chan_read_perm),
    {ok, Write} = application:get_env(monitor, master_chan_write_perm),
    {atomic, _} = init_targets(),
    {atomic, _} = init_probes(),
    {atomic, _} = init_jobs(),
    {ok, #state{perm=#perm_conf{read=Read,write=Write}}}.

%%----------------------------------------------------------------------------
%% SUPERCAST_CHANNEL BEHAVIOUR CALLS
%%----------------------------------------------------------------------------
handle_call(get_perms, _F, #state{perm = P} = S) ->
    {reply, P, S};

%%----------------------------------------------------------------------------
%% API CALLS
%%----------------------------------------------------------------------------
handle_call({create_probe, Probe}, _F, S) ->
    monitor_probe_sup:new(Probe),
    monitor_data:write_probe(Probe),
    {reply, ok, S};

handle_call({create_target, Target}, _F, S) ->
    init_target(Target),
    monitor_data:write_target(Target),
    {reply, ok, S}.

%%----------------------------------------------------------------------------
%% SUPERCAST_CHANNEL BEHAVIOUR CASTS
%%----------------------------------------------------------------------------
handle_cast({sync_request, CState}, S) ->
    supercast_channel:subscribe(?MASTER_CHANNEL, CState),

    {atomic, _Targets} = monitor_data:iterate_target_table(fun(X,Acc) ->
        #target{global_perm=Perm} = X,
        case supercast:satisfy(CState, Perm) of
            true    ->
                [X|Acc];
            false   ->
                Acc
        end
    end),

    {atomic, _Probes} = monitor_data:iterate_probe_table(fun(X,Acc) ->
        #probe{permissions=Perm} = X,
        case supercast:satisfy(CState, Perm) of
            true    ->
                [X|Acc];
            false   ->
                Acc
        end
    end),

    {atomic, _Jobs} = monitor_data:iterate_job_table(fun(X,Acc) ->
        #job{permissions=Perm} = X,
        case supercast:satisfy(CState, Perm) of
            true    ->
                [X|Acc];
            false   ->
                Acc
        end
    end),


    ?LOG({should_send, _Targets, _Probes, _Jobs}),
    ?LOG({should_trigger_return, _Probes}),
    %{Pdus, Probes} = gen_dump_pdus(CState, Targets),
    %supercast_channel:unicast(CState, Pdus),
    %lists:foreach(fun(X) ->
    %    monitor_probe:triggered_return(X, CState)
    %end, Probes),
 
    {noreply, S};

handle_cast(_R, S) ->
    {noreply, S}.

handle_info(_, S) ->
    {noreply, S}.

terminate(_R, _) ->
    normal.

code_change(_O, S, _E) ->
    {ok, S}.


%%----------------------------------------------------------------------------
%% UTILS    
%%----------------------------------------------------------------------------
init_target(Target) ->
    Dir = proplists:get_value(var_directory, Target#target.sys_properties),
    case file:read_file_info(Dir) of
        {ok, _} ->
            ok;
        {error, enoent} ->
            file:make_dir(Dir);
        Other ->
            exit({error, Other})
    end,
    ok  = monitor_snmp_utils:init_snmp_conf(Target).
    
init_targets() ->
    monitor_data:iterate_target_table(fun(Target,_) ->
        monitor_snmp_utils:init_snmp_conf(Target)
    end).

init_probes() ->
    monitor_data:iterate_probe_table(fun(X,_) ->
        monitor_probe_sup:new(X)
    end).

init_jobs() ->
    monitor_data:iterate_job_table(fun(X,_) ->
        io:format("~p~n",[X])
    end).

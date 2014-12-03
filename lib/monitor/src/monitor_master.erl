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
    create_probe/1
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
    {ok, Read}  = application:get_env(monitor, master_chan_read_perm),
    {ok, Write} = application:get_env(monitor, master_chan_write_perm),
    {ok,_} = init_targets(),
    {ok,_} = init_probes(),
    {ok,_} = init_jobs(),
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
    monitor_data:write_probe(Probe),
    monitor_probe_sup:launch(Probe),
    {reply, ok, S};

handle_call({create_target, Target}, _F, S) ->
    monitor_data:write_target(Target),
    monitor_utils:init_target_snmp(Target),
    monitor_utils:init_target_dir(Target),
    {reply, ok, S}.

%%----------------------------------------------------------------------------
%% SUPERCAST_CHANNEL BEHAVIOUR CASTS
%%----------------------------------------------------------------------------
handle_cast({sync_request, CState}, S) ->
    supercast_channel:subscribe(?MASTER_CHANNEL, CState),

    {atomic, _Targets} = monitor_data:iterate_target_table(fun(T,_) ->
        #target{permissions=Perm} = T,
        case supercast:satisfy(CState, Perm) of
            true    ->
                Pdu = monitor_pdu:'PDU-MonitorPDU-fromServer-infoTarget-create'(T),
                ok  = supercast_channel:unicast(CState, [Pdu]);
            false   -> ok
        end
    end),

    {atomic, _Probes} = monitor_data:iterate_probe_table(fun(P,_) ->
        #probe{permissions=Perm} = P,
        case supercast:satisfy(CState, Perm) of
            true    ->
                Pdu = monitor_pdu:'PDU-MonitorPDU-fromServer-infoProbe-create'(P),
                ok  = supercast_channel:unicast(CState, [Pdu]),
                monitor_probe:triggered_return(P#probe.name, CState);
            false   -> ok
        end
    end),

    {atomic, _Jobs} = monitor_data:iterate_job_table(fun(J,Acc) ->
        #job{permissions=Perm} = J,
        case supercast:satisfy(CState, Perm) of
            true    ->
                [J|Acc];
            false   ->
                Acc
        end
    end),


    ?LOG({should_send_jobs, _Jobs}),

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
init_targets() ->
    {atomic, R} = monitor_data:iterate_target_table(fun(T,_) ->
        monitor_utils:init_target_snmp(T),
        monitor_utils:init_target_dir(T)
    end),
    {ok, R}.

init_probes() ->
    {atomic, R} = monitor_data:iterate_probe_table(fun(P,_) ->
        monitor_probe_sup:launch(P)
    end),
    {ok, R}.

init_jobs() ->
    {atomic, R} = monitor_data:iterate_job_table(fun(J,_) ->
        io:format("~p~n",[J])
    end),
    {ok, R}.

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
-module(tracker_master_channel).
-behaviour(gen_server).
-behaviour(gen_channel).
-include("../include/tracker.hrl").

-export([
    init/1,
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

-export([
    start_link/1,
    chan_add/1,
    chan_del/1,
    chan_update/1,
    probe_update/2,
    dump/0
]).

-record(state, {
    chans,
    perm,
    probe_modules
}).

-define(MASTER_CHAN, 'target-MasterChan').


%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
% @private
-spec start_link([any()]) -> {ok, pid()}.
start_link(ProbeModules) ->
    gen_server:start_link({local, ?MASTER_CHAN}, ?MODULE, [ProbeModules], []).


%%----------------------------------------------------------------------------
%% API for the tracker_target_channel(s) modules
%%----------------------------------------------------------------------------

-spec chan_add(#target{}) -> ok.
% @doc
% Called by a target_channel at initialisation stage.
% @end
chan_add(Target) ->
    io:format("Chan add ~p~n", [?MODULE]),
    gen_server:call(?MASTER_CHAN, {chan_update, Target}).

    
-spec chan_del(#target{}) -> ok.
% @doc
% Called by a target_channel at termination stage.
% @end
chan_del(Target) ->
    io:format("Chan del ~p~n", [?MODULE]),
    gen_server:call(?MASTER_CHAN, {chan_del, Target}).

% @doc
% Called by a tracker_target_channel when information must be forwarded
% to subscribers of 'target-MasterChan'.
% @end
chan_update(NewTarget) ->
    gen_server:call(?MASTER_CHAN, {chan_update, NewTarget}).

% @doc
% Called by a tracker_target_channel when information must be forwarded
% to subscribers of 'target-MasterChan' concerning one of the probes.
% @end
probe_update(NewTarget, NewProbe) ->
    gen_server:call(?MASTER_CHAN, {probe_update, NewTarget, NewProbe}).

dump() ->
    gen_server:call(?MASTER_CHAN, dump).

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------


%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% INIT
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
init([ProbeModules]) ->
    P = extract_probes_info(ProbeModules),
    {ok, #state{
            chans = [],
            perm = #perm_conf{
                read    = ["admin", "wheel"],
                write   = ["admin"]
            },
            probe_modules = P
        }
    }.
    
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% HANDLE_CALL
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------

%%----------------------------------------------------------------------------
%% SELF API CALLS
%%----------------------------------------------------------------------------
handle_call({probe_update, 
        #target{id = TargetId}      = NewTarget, 
        #probe{permissions = Perm}  = NewProbe
    }, _F, #state{chans = Chans} = S) ->
    NewChans = lists:keyreplace(TargetId, 2, Chans, NewTarget),
    supercast_mpd:multicast_msg(?MASTER_CHAN, {Perm, 
        pdu(probeInfo, {update, TargetId, NewProbe})}),
    {reply, ok, S#state{chans = NewChans}};

handle_call({chan_update, #target{id = Id, global_perm = Perm} = Target}, _F, 
        #state{chans = C} = S) ->
    case lists:keyfind(Id, 2, C) of
        false ->    % did not exist insert
            supercast_mpd:multicast_msg(?MASTER_CHAN, {Perm,
                pdu(targetInfo, Target)}),
            {reply, ok, S#state{
                    chans = [Target | C]
                }
            };
        _ -> % exist update
            supercast_mpd:multicast_msg(?MASTER_CHAN, {Perm,
                pdu(targetInfo, Target)}),
            {reply, ok, 
                S#state{
                    chans = lists:keyreplace(Id, 2, C, Target)
                }
            }
    end;

handle_call({chan_del, #target{id = Id, global_perm = Perm}}, _F, 
        #state{chans = C} = S) ->
    supercast_mpd:multicast_msg(?MASTER_CHAN, {Perm, pdu(targetDelete, Id)}),
    {reply, ok, S#state{chans = lists:keydelete(Id, 2, C)}};

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% CALLS VIA GEN_CHANNEL BEHAVIOUR
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
% These calls are used by the gen_channel behaviour module
% Called by supercast_mpd via gen_channel to allow or not a client to subscribe in 
% regard of the result after applying beha_supercast_ctrl:satisfy/3.
handle_call(get_perms, _F, #state{perm = P} = S) ->
    {reply, P, S};

handle_call({synchronize, #client_state{module = CMod} = CState}, 
        _F, #state{chans = Chans, probe_modules = PMods} = State) ->
    % subscribe the client to mpd,
    supercast_mpd:subscribe_stage3(?MASTER_CHAN, CState),
    PMList  = [pdu(probeModInfo, Probe) || Probe <- PMods],
    % gen_dump_pdus will filter based on CState permissions
    PDUs    = gen_dump_pdus(CState, Chans),
    PTotal  = lists:append(PMList, PDUs),
    PSend   = [{CState, Pdu} || Pdu <- PTotal],
    lists:foreach(fun({C_State, Pdu}) ->
        CMod:send(C_State, Pdu)
    end, PSend),
    {reply, ok, State};

handle_call(dump, _F, State) ->
    {reply, State, State}.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% HANDLE_CAST
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
handle_cast(_R, S) ->
    {noreply, S}.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% HANDLE_INFO
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
handle_info(I, S) ->
    io:format("handle_info ~p ~p ~p~n", [?MODULE, I, S]),
    {noreply, S}.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% TERMINATE  
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
terminate(_R, _S) ->
    normal.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% CODE_CHANGE
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
code_change(_O, S, _E) ->
    {ok, S}.

%%----------------------------------------------------------------------------
%% PDU BUILD
%%----------------------------------------------------------------------------
pdu(targetInfo, #target{id = Id, properties = Prop}) ->
    AsnProp = lists:foldl(fun({X,_Y}, Acc) ->
        %[{'TargetProperty', atom_to_list(X), io_lib:format("~p", [Y])} | Acc]
        [{'Property', atom_to_list(X), "hello"} | Acc]
    end, [], Prop),
    {modTrackerPDU,
        {fromServer,
            {targetInfo,
                {'TargetInfo',
                    atom_to_list(Id),
                    AsnProp,
                    create}}}};

pdu(targetDelete, Id) ->
    {modTrackerPDU,
        {fromServer,
            {targetInfo,
                {'TargetInfo',
                    atom_to_list(Id),
                    [],
                    delete}}}};

pdu(probeInfo, {InfoType, Id, 
        #probe{
            permissions = #perm_conf{read = R, write = W},
            tracker_probe_conf = ProbeConf
        } = Probe
    }) ->
    P = {modTrackerPDU,
        {fromServer,
            {probeInfo,
                {'ProbeInfo',
                    atom_to_list(Id),
                    Probe#probe.id,
                    atom_to_list(Probe#probe.name),
                    {'PermConf', R, W},
                    atom_to_list(Probe#probe.tracker_probe_mod),
                    gen_asn_probe_conf(ProbeConf),
                    atom_to_list(Probe#probe.status),
                    Probe#probe.timeout,
                    Probe#probe.step,
                    gen_asn_probe_inspectors(Probe#probe.inspectors),
                    gen_asn_probe_loggers(Probe#probe.loggers),
                    gen_asn_probe_properties(Probe#probe.properties),
                    gen_asn_probe_active(Probe#probe.active),
                    InfoType}}}},
    P;

pdu(probeModInfo,  {ProbeName, ProbeInfo}) ->
    {modTrackerPDU,
        {fromServer,
            {probeModInfo,
                {'ProbeModuleInfo',
                    atom_to_list(ProbeName),
                    ProbeInfo }}}};

pdu(probeActivity, {TargetId, ProbeId, PState, Msg, ReturnStatus, Time}) ->
    {modTrackerPDU,
        {fromServer,
            {probeActivity,
                {'ProbeActivity',
                    atom_to_list(TargetId),
                    ProbeId,
                    Time,
                    atom_to_list(PState),
                    atom_to_list(ReturnStatus),
                    Msg}}}}.


%%----------------------------------------------------------------------------
%% UTILS    
%%----------------------------------------------------------------------------
extract_probes_info(ProbeModules) ->
    lists:foldl(
        fun(PMod, Acc) ->
            {ok, Info} = PMod:info(),
            [{PMod, Info} | Acc] 
        end, 
    [], ProbeModules).

gen_asn_probe_active(true) ->
    1;
gen_asn_probe_active(false) ->
    0.

gen_asn_probe_conf(Conf) when is_record(Conf, nagios_plugin_conf) ->
    #nagios_plugin_conf{executable = Exe, args = Args} = Conf,
    lists:flatten([Exe, " ", [[A, " ", B, " "] || {A, B} <- Args]]);

gen_asn_probe_conf(Conf) when is_record(Conf, snmp_conf) ->
    lists:flatten(io_lib:format("~p", [Conf])).

gen_asn_probe_inspectors(Inspectors) ->
    [{
        'Inspector',
        atom_to_list(Module),
        lists:flatten(io_lib:format("~p", [Conf]))
    } || {_, Module, Conf} <- Inspectors].

gen_asn_probe_loggers(Loggers) ->
    [gen_logger_pdu(LConf) || LConf <- Loggers].

gen_logger_pdu({logger, btracker_logger_text = N, Cfg}) ->
    {loggerText, {'LoggerText', atom_to_list(N), to_string(Cfg)}};
gen_logger_pdu({logger, btracker_logger_rrd = N, Cfg}) ->
    {create, Create} = lists:keyfind(create, 1, Cfg),
    {update, Update} = lists:keyfind(update, 1, Cfg),
    {graphs,  Graph} = lists:keyfind(graphs, 1, Cfg),
    {binds,   Binds} = lists:keyfind(binds,  1, Cfg),
    {loggerRrd, {'LoggerRrd',
        atom_to_list(N),
        Create,
        Update,
        Graph,
        [{'Bind', Rep, Mac} || {Rep, Mac} <- Binds]
        }
    }.

%gen_asn_probe_properties(Properties) ->
    %[{'Property', Key, Value} 
        %|| {Key,Value} <- Properties].

gen_dump_pdus(CState, Targets) ->
    FTargets    = supercast_mpd:filter_things(CState, [{Perm, Target} ||
        #target{global_perm = Perm} = Target <- Targets]),
    TargetsPDUs = [pdu(targetInfo, Target) || Target <- FTargets],
    ProbesDefs  = [{TId, Probes} ||
        #target{id = TId, probes = Probes} <- FTargets],
    gen_dump_pdus(CState, TargetsPDUs, [], ProbesDefs).
gen_dump_pdus(_, TargetsPDUs, ProbesPDUs, []) ->
    lists:append(TargetsPDUs, ProbesPDUs);
gen_dump_pdus(CState, TargetsPDUs, ProbesPDUs, [{TId, Probes}|T]) ->
    ProbesThings  = [{Perm, Probe} || 
        #probe{permissions = Perm} = Probe <- Probes],
    AllowedThings = supercast_mpd:filter_things(CState, ProbesThings),
    Result = [pdu(probeInfo, {create, TId, Probe}) ||
        Probe <- AllowedThings],
    gen_dump_pdus(CState, TargetsPDUs, lists:append(ProbesPDUs, Result), T).

to_string(Term) ->
    lists:flatten(io_lib:format("~p", [Term])).

gen_asn_probe_properties(K) ->
    gen_asn_probe_properties(K, []).
gen_asn_probe_properties([], S) ->
    S;
gen_asn_probe_properties([{K,V} | T], S) when is_list(V) ->
    gen_asn_probe_properties(T, [{'Property', K, V} | S]);
gen_asn_probe_properties([{K,V} | T], S) when is_integer(V) ->
    gen_asn_probe_properties(T, [{'Property', K, integer_to_list(V)} | S]);
gen_asn_probe_properties([{K,V} | T], S) when is_float(V) ->
    gen_asn_probe_properties(T, [{'Property', K, float_to_list(V, [{decimals, 10}])} | S]);
gen_asn_probe_properties([{K,V} | T], S) when is_atom(V) ->
    gen_asn_probe_properties(T, [{'Property', K, atom_to_list(V)} | S]).

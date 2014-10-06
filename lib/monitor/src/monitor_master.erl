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
    code_change/3,
    dump/0
]).

% GEN_CHANNEL
-export([
    get_perms/1,
    sync_request/2
]).

% API
-export([
    start_link/2,
    probe_info/2,
    probe_activity/3,
    create_target/1,
    create_probe/2,
    id_used/1
]).

-record(state, {
    chans,
    perm,
    probe_modules,
    db
}).

-define(DETS_TARGETS, 'targets_db').
-define(MASTER_CHAN, 'target-MasterChan').


-spec start_link(ProbeModules::[tuple()], ConfFile::string()) -> {ok, pid()}.
start_link(PMods, CFile) ->
    gen_server:start_link({local, ?MASTER_CHAN}, ?MODULE, [PMods, CFile], []).

dump() ->
    gen_server:call(?MASTER_CHAN, dump_dets).

-spec create_target(Target::#target{}) -> ok | {error, Info::string()}.
create_target(Target) ->
    gen_server:call(?MASTER_CHAN, {create_target, Target}).

-spec create_probe(TargetId::atom(), Probe::#probe{}) 
    -> ok | {error, string()}.
create_probe(TargetId, Probe) ->
    gen_server:call(?MASTER_CHAN, {create_probe, TargetId, Probe}).

%%----------------------------------------------------------------------------
%% supercast_channel API
%%----------------------------------------------------------------------------
-spec get_perms(PidName::atom()) -> {ok, PermConf::#perm_conf{}}.
get_perms(PidName) ->
    gen_server:call(PidName, get_perms).

sync_request(PidName, CState) ->
    gen_server:cast(PidName, {sync_request, CState}).


%%----------------------------------------------------------------------------
%% monitor_probe_fsm API
%%----------------------------------------------------------------------------
-spec probe_info(TargetId::atom(), Probe::#probe{}) -> ok.
% @doc
% Called by a monitor_probe_fsm when information must be forwarded
% to subscribers of 'target-MasterChan' concerning the probe.
% @end
probe_info(TargetId, Probe) ->
    gen_server:cast(?MASTER_CHAN, {probe_info, TargetId, Probe}).

-spec id_used(Id::atom()) -> true | false.
% @doc
% Used by monitor_commander:generate_id to check the presence of a randomly
% created id.
% @end
id_used(Id) ->
    gen_server:call(?MASTER_CHAN, {id_used, Id}).

-spec probe_activity(Target::atom(), Probe::#probe{}, Return::#probe_return{}) ->
    ok.
probe_activity(Target, Probe, Return) ->
    gen_server:cast(?MASTER_CHAN, {probe_activity, Target, Probe, Return}).



%%----------------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%----------------------------------------------------------------------------
init([ProbeModules, _ConfFile]) ->
    {ok, Table} = init_database(),
    P   = extract_probes_info(ProbeModules),
    %{ok, Targets} = load_targets_conf_from_file(ConfFile),
    {ok, Targets} = load_targets_conf_from_dets(Table),
    {ok, #state{
            chans = Targets,
            perm = #perm_conf{
                read    = ["admin", "wheel"],
                write   = ["admin"]
            },
            probe_modules = P,
            db          = Table
        }
    }.
    
init_database() ->
    {ok, VarDir} = application:get_env(monitor, targets_data_dir),
    DetsFile     = filename:absname_join(VarDir, "targets.dets"),
    case filelib:is_file(DetsFile) of
        true  ->
            case dets:is_dets_file(DetsFile) of
                true ->
                    dets:open_file(DetsFile);
                false ->
                    ok = file:delete(DetsFile),
                    dets:open_file(DetsFile)
            end;
        false ->
            {ok, N} = dets:open_file(?DETS_TARGETS, [
                {file,   DetsFile},
                {keypos, 2},
                {ram_file, false},
                {auto_save, 180000},
                {type, set}
            ]),
            dets:close(N),
            dets:open_file(DetsFile)
    end.
%%----------------------------------------------------------------------------
%% SELF API CALLS
%%----------------------------------------------------------------------------
% handle_call({chan_update, #target{id = Id, global_perm = Perm} = Target}, _F, 
%         #state{chans = C} = S) ->
%     case lists:keyfind(Id, 2, C) of
%         false ->    % did not exist insert
%             supercast_channel:emit(?MASTER_CHAN, {Perm,
%                 pdu(targetInfo, Target)}),
%             {reply, ok, S#state{
%                     chans = [Target | C]
%                 }
%             };
%         _ -> % exist update
%             supercast_channel:emit(?MASTER_CHAN, {Perm,
%                 pdu(targetInfo, Target)}),
%             {reply, ok, 
%                 S#state{
%                     chans = lists:keyreplace(Id, 2, C, Target)
%                 }
%             }
%     end;
% 
% handle_call({chan_del, #target{id = Id, global_perm = Perm}}, _F, 
%         #state{chans = C} = S) ->
%     supercast_channel:emit(?MASTER_CHAN, {Perm, pdu(targetDelete, Id)}),
%     {reply, ok, S#state{chans = lists:keydelete(Id, 2, C)}};

%%----------------------------------------------------------------------------
%% SUPERCAST_CHANNEL BEHAVIOUR CALLS
%%----------------------------------------------------------------------------
handle_call({id_used, Id}, _F, #state{chans=Chans} = S) ->
    TargetAtomList  = [Tid    || #target{id=Tid} <- Chans],
    Probes          = [Probes || #target{probes=Probes} <- Chans],
    ProbeAtomList   = [PrId   || #probe{name=PrId} <- lists:flatten(Probes)],
    TotalAtomList   = lists:append(TargetAtomList, ProbeAtomList),
    Rep = lists:member(Id, TotalAtomList),
    {reply, Rep, S};

handle_call(get_perms, _F, #state{perm = P} = S) ->
    {reply, P, S};

handle_call({create_probe, TargetId, Probe}, _F, S) ->
    % is targetId a valid atom?
    case (catch erlang:list_to_existing_atom(TargetId)) of
        {'EXIT', _} ->
            Msg = lists:flatten(
                io_lib:format("Target ~s did not exist",[TargetId])
            ),
            {reply, {error, Msg}, S};
        TargetAtom ->
            Table = S#state.db,
            case dets:lookup(Table, TargetAtom) of
                {error, Reason} ->
                    {reply, {error, Reason}, S};
                [TargetRecord] ->
                    {NProbe, NTarget} = insert_probe(Probe, TargetRecord),
                    ProbeInfoPdu = pdu(probeInfo, {create, TargetAtom, NProbe}),
                    Perm = NProbe#probe.permissions,
                    supercast_channel:emit(?MASTER_CHAN, {Perm, ProbeInfoPdu}),
                    dets:insert(Table, NTarget),
                    monitor_event_manager:notify({create_probe, NTarget}),
                    {reply, ok, S};
                _ ->
                    {reply, {error, "Key error"}, S}
            end
    end;

handle_call({create_target, Target}, _F, S) ->
    Target2 = load_target_conf(Target),
    emit_wide(Target2),
    monitor_event_manager:notify({new_target, Target2}),
    dets:insert(S#state.db, Target2),

    Targets = S#state.chans,
    S2      = S#state{chans = [Target2|Targets]},
    {reply, ok, S2};

handle_call(dump_dets, _F, S) ->
    Table = S#state.db,
    TargetsConf = dets:foldr(fun(X, Acc) ->
        [X|Acc]
    end, [], Table),
    ?LOG(TargetsConf),
    {reply, ok, S}.

insert_probe(Probe, Target) ->
    Probes = Target#target.probes,
    Id = get_free_probe_id(Probes),
    P1 = Probe#probe{id = Id},
    T1 = Target#target{probes = [P1|Probes]},
    {ok, Pid} = monitor_probe_sup:new({Target, P1}),
    P2 = P1#probe{pid = Pid},
    T2 = T1#target{probes = [P2|Probes]},
    {P2, T2}.

get_free_probe_id(Probes) ->
    Ids = [P#probe.id || P <- Probes],
    get_free_probe_id(0, Ids).
get_free_probe_id(N, Ids) ->
    case lists:member(N, Ids) of
        true ->
            get_free_probe_id(N + 1, Ids);
        false ->
            N
    end.

emit_wide(Target) ->
    Perm        = Target#target.global_perm,
    PduTarget   = pdu(targetInfo, Target),
    supercast_channel:emit(?MASTER_CHAN, {Perm, PduTarget}),

    TId         = Target#target.id,
    Probes      = Target#target.probes,
    PduProbes   = [{ProbePerm, pdu(probeInfo, {create, TId, Probe})} || 
        #probe{permissions = ProbePerm} = Probe <- Probes],
    lists:foreach(fun(X) ->
        supercast_channel:emit(?MASTER_CHAN, X)
    end, PduProbes).




%%----------------------------------------------------------------------------
%% SUPERCAST_CHANNEL BEHAVIOUR CASTS
%%----------------------------------------------------------------------------
handle_cast({sync_request, CState}, State) ->
    Chans   = State#state.chans,
    PMods   = State#state.probe_modules,
    % I want this client to receive my messages,
    supercast_channel:subscribe(?MASTER_CHAN, CState),
    PMList  = [pdu(probeModInfo, Probe) || Probe <- PMods],
    % gen_dump_pdus will filter based on CState permissions
    PDumps  = gen_dump_pdus(CState, Chans),
    Pdus    = lists:append(PMList, PDumps),
    supercast_channel:unicast(CState, Pdus),
    {noreply, State};

%%----------------------------------------------------------------------------
%% SELF API CASTS
%%----------------------------------------------------------------------------
handle_cast({probe_info, TargetId, NewProbe}, S) ->
    monitor_event_manager:notify({probe_info, TargetId, NewProbe}),
    Chans       = S#state.chans,
    Perms       = NewProbe#probe.permissions,
    Target      = lists:keyfind(TargetId, 2, Chans),
    {ok, NewTarg} = update_info_chan(Target, NewProbe),
    NewChans      = lists:keystore(TargetId, 2, Chans, NewTarg),
    ?LOG('probe_info'),
    dets:insert(S#state.db, NewTarg),

    Pdu = pdu(probeInfo, {update, TargetId, NewProbe}),
    supercast_channel:emit(?MASTER_CHAN, {Perms, Pdu}),
    NS = S#state{chans = NewChans},
    {noreply, NS};

handle_cast({probe_activity, Target, Probe, Return}, S) ->
    monitor_event_manager:notify({probe_activity, Target, Probe, Return}),
    Pdu = pdu(probeActivity, {
        Target#target.id,
        Probe#probe.name,
        none,
        Return#probe_return.original_reply,
        Return#probe_return.status,
        Return#probe_return.timestamp
    }),
    supercast_channel:emit(?MASTER_CHAN, {Probe#probe.permissions, Pdu}),
    {noreply, S};

handle_cast(_R, S) ->
    {noreply, S}.

handle_info(_, S) ->
    {noreply, S}.

terminate(_R, #state{db = D}) ->
    dets:close(D),
    normal.

code_change(_O, S, _E) ->
    {ok, S}.


%%----------------------------------------------------------------------------
%% UTILS    
%%----------------------------------------------------------------------------
update_info_chan(Target, Probe) ->
    TProperties     = Target#target.properties,
    PProperties     = Probe#probe.properties,
    PForward        = Probe#probe.forward_properties,

    {ok, NewTProp} = 
        probe_property_forward(TProperties, PProperties, PForward),


    Probes  = Target#target.probes,
    PrId    = Probe#probe.id,
    % TODO use #probe.name instead of #probe.id
    NProbes = lists:keystore(PrId, 2, Probes, Probe),
    NTarget = Target#target{
        probes      = NProbes,
        properties  = NewTProp
    },
    case NewTProp of
        TProperties ->
            % nothing to do
            ok;
        _ ->
            % update target properties state to the client
            TargetPerm  = NTarget#target.global_perm,
            Pdu         = pdu(targetInfo, NTarget),
            supercast_channel:emit(?MASTER_CHAN, {TargetPerm, Pdu})
    end,
    %NChans  = lists:keystore(TargetId, 2, Chans, NTarget),
    {ok, NTarget}.

probe_property_forward(TProperties, _, []) ->
    {ok, TProperties};
probe_property_forward(TProperties, PProperties, [P|Rest]) ->
    case lists:keyfind(P, 1, PProperties) of
        false ->
            probe_property_forward(TProperties, PProperties, Rest);
        {P, Val} ->
            NTProperties = lists:keystore(P,1,TProperties, {P,Val}),
            probe_property_forward(NTProperties, PProperties, Rest)
    end.

    

load_targets_conf_from_dets(Table) ->
    TargetsConf = dets:foldr(fun(X, Acc) ->
        [X|Acc]
    end, [], Table),
    TargetsState = [],
    load_targets_conf(TargetsConf, TargetsState).

%load_targets_conf_from_file(TargetsConfFile) ->
    %{ok, TargetsConf}  = file:consult(TargetsConfFile),
    %TargetsState = [],
    %load_targets_conf(TargetsConf, TargetsState).

load_targets_conf([], TargetsState) ->
    {ok, TargetsState};
load_targets_conf([T|Targets], TargetsState) ->
    T2 = load_target_conf(T),
    load_targets_conf(Targets, [T2|TargetsState]).
    
load_target_conf(Target) ->
    io:format("ninit probes?"),
    ok              = init_target_dir(Target),
    {ok, Target2}   = init_probes(Target),
    Target2.

init_probes(Target) ->
    ProbesOrig  = Target#target.probes,
    ProbesNew   = [],
    init_probes(Target, ProbesOrig, ProbesNew).
init_probes(Target, [], ProbesNew) ->
    TargetNew = Target#target{probes = ProbesNew},
    {ok, TargetNew};
init_probes(Target, [P|Probes], ProbesN) ->
    {ok, Pid} = monitor_probe_sup:new({Target, P}),
    NP        = P#probe{pid = Pid},
    ProbesN2  = [NP|ProbesN],
    init_probes(Target, Probes, ProbesN2).

init_target_dir(Target) ->
    Dir = Target#target.directory,
    case file:read_file_info(Dir) of
        {ok, _} ->
            ok;
        {error, enoent} ->
            file:make_dir(Dir);
        Other ->
            {error, Other}
    end.

extract_probes_info(ProbeModules) ->
    lists:foldl(
        fun(PMod, Acc) ->
            {ok, Info} = PMod:info(),
            [{PMod, Info} | Acc] 
        end, 
    [], ProbeModules).

%%----------------------------------------------------------------------------
%% PDU BUILD
%%----------------------------------------------------------------------------
pdu(targetInfo, #target{id=Id, properties=Prop}) ->
    AsnProps = lists:foldl(fun({K,V}, Acc) ->
        [{'Property', K, V} | Acc]
    end, [], Prop),
    {modMonitorPDU,
        {fromServer,
            {targetInfo,
                {'TargetInfo',
                    atom_to_list(Id),
                    AsnProps,
                    create}}}};

pdu(targetInfoUpdate, #target{id=Id, properties=Prop}) ->
    AsnProps = lists:foldl(fun({K,V}, Acc) ->
        [{'Property', K, V} | Acc]
    end, [], Prop),
    {modMonitorPDU,
        {fromServer,
            {targetInfo,
                {'TargetInfo',
                    atom_to_list(Id),
                    AsnProps,
                    update}}}};

pdu(targetInfoDelete, Id) ->
    {modMonitorPDU,
        {fromServer,
            {targetInfo,
                {'TargetInfo',
                    atom_to_list(Id),
                    [],
                    delete}}}};

pdu(probeInfo, {InfoType, Id, 
        #probe{
            permissions         = #perm_conf{read = R, write = W},
            monitor_probe_conf  = ProbeConf,
            description         = Descr,
            info                = Info
        } = Probe
    }) ->
    P = {modMonitorPDU,
        {fromServer,
            {probeInfo,
                {'ProbeInfo',
                    atom_to_list(Id),
                    Probe#probe.id,
                    atom_to_list(Probe#probe.name),
                    Descr,
                    Info,
                    {'PermConf', R, W},
                    atom_to_list(Probe#probe.monitor_probe_mod),
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
    {modMonitorPDU,
        {fromServer,
            {probeModInfo,
                {'ProbeModuleInfo',
                    atom_to_list(ProbeName),
                    ProbeInfo }}}};

pdu(probeActivity, {TargetId, ProbeName, PState, Msg, ReturnStatus, Time}) ->
    {modMonitorPDU,
        {fromServer,
            {probeActivity,
                {'ProbeActivity',
                    atom_to_list(TargetId),
                    atom_to_list(ProbeName),
                    Time,
                    atom_to_list(PState),
                    atom_to_list(ReturnStatus),
                    Msg}}}}.


gen_asn_probe_active(true)  -> 1;
gen_asn_probe_active(false) -> 0.

gen_asn_probe_conf(Conf) when is_record(Conf, ncheck_probe_conf) ->
    #ncheck_probe_conf{executable = Exe, args = Args} = Conf,
    lists:flatten([Exe, " ", [[A, " ", B, " "] || {A, B} <- Args]]);

gen_asn_probe_conf(Conf) when is_record(Conf, nagios_probe_conf) ->
    #nagios_probe_conf{executable = Exe, args = Args} = Conf,
    lists:flatten([Exe, " ", [[A, " ", B, " "] || {A, B} <- Args]]);

gen_asn_probe_conf(Conf) when is_record(Conf, snmp_probe_conf) ->
    lists:flatten(io_lib:format("~p", [Conf])).

gen_asn_probe_inspectors(Inspectors) ->
    [{
        'Inspector',
        atom_to_list(Module),
        lists:flatten(io_lib:format("~p", [Conf]))
    } || {_, Module, Conf} <- Inspectors].

gen_asn_probe_loggers(Loggers) ->
    [gen_logger_pdu(LConf) || LConf <- Loggers].

gen_logger_pdu({logger, bmonitor_logger_rrd, Cfg}) ->
    {loggerRrd, 
        {'LoggerRrd',
            atom_to_list(bmonitor_logger_rrd),
            gen_rrd_configs(Cfg)
        }
    };
gen_logger_pdu({logger, bmonitor_logger_text, Cfg}) ->
    {loggerText, 
        {'LoggerText', 
            atom_to_list(bmonitor_logger_text), 
            to_string(Cfg)}}.

gen_rrd_configs(Cfg) ->
    gen_rrd_configs(Cfg, []).
gen_rrd_configs([], Ret) ->
    Ret;
gen_rrd_configs([H|T], Ret) ->
    Conf = {'RrdConfig',
        H#rrd_config.file,
        H#rrd_config.create,
        H#rrd_config.update,
        H#rrd_config.graphs,
        [{'Bind', Repl, Macro} || {Repl, Macro} <- H#rrd_config.binds]
    },
    gen_rrd_configs(T, [Conf | Ret]).

%gen_asn_probe_properties(Properties) ->
    %[{'Property', Key, Value} 
        %|| {Key,Value} <- Properties].

gen_dump_pdus(CState, Targets) ->
    FTargets    = supercast:filter(CState, [{Perm, Target} ||
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
    AllowedThings = supercast:filter(CState, ProbesThings),
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

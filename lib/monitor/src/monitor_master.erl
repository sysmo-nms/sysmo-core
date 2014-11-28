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
    start_link/0,

    probe_info/2,

    create_target/1,
    create_probe/2,

    job_update_properties/2,

    generate_id/1
]).

-record(state, {
    perm,
    dets_ref
}).

% generate id range
% 1 000 000 possible values
-define(RAND_RANGE, 1000000).
% but must be a minimum of 100000
-define(RAND_MIN,   99999).

-define(DETS_TARGETS, 'targets_db').


-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MASTER_CHANNEL}, ?MODULE, [], []).

dump() ->
    gen_server:call(?MASTER_CHANNEL, dump_dets).

%% monitor_commander API
-spec create_target(Target::#target{}) -> ok | {error, Info::string()}.
% @doc
% Called from monitor_commander.
% @end
create_target(Target) ->
    gen_server:call(?MASTER_CHANNEL, {create_target, Target}).

-spec create_probe(TargetId::atom(), Probe::#probe{}) 
    -> ok | {error, string()}.
create_probe(TargetId, Probe) ->
    gen_server:call(?MASTER_CHANNEL, {create_probe, TargetId, Probe}).

-spec generate_id(Head::string()) -> atom().
% @doc
% Called from monitor_commander.
% @end
generate_id(Head) ->
    Int         = random:uniform(?RAND_RANGE),
    RandId      = Int + ?RAND_MIN,
    RandIdL     = io_lib:format("~p", [RandId]),
    RandIdS     = lists:flatten(RandIdL),
    RandIdF     = lists:concat([Head, RandIdS]),
    ToAtom      = erlang:list_to_atom(RandIdF),
    case gen_server:call(?MASTER_CHANNEL, {id_used, ToAtom}) of
        false->  {ok, ToAtom};
        true  -> generate_id(Head)
    end.

%%----------------------------------------------------------------------------
%% supercast_channel API
%%----------------------------------------------------------------------------
-spec get_perms(PidName::atom()) -> {ok, PermConf::#perm_conf{}}.
get_perms(PidName) ->
    gen_server:call(PidName, get_perms).

-spec sync_request(PidName::atom(), CState::tuple()) ->  ok.
sync_request(PidName, CState) ->
    gen_server:cast(PidName, {sync_request, CState}).


%%----------------------------------------------------------------------------
%% monitor_probe API
%%----------------------------------------------------------------------------
-spec probe_info(TargetId::atom(), Probe::#probe{}) -> ok.
% @doc
% Called by a monitor_probe when information must be forwarded
% to subscribers of ?MASTER_CHANNEL concerning the probe.
% @end
probe_info(TargetId, Probe) ->
    gen_server:cast(?MASTER_CHANNEL, {probe_info, TargetId, Probe}).

%%----------------------------------------------------------------------------
%% monitor_jobs API    
%%----------------------------------------------------------------------------
-spec job_update_properties(TargetId::atom(), Properties::[{string(),any()}]) ->
    ok.
% @doc
% Called by various monitor_jobs functions to update target properties.
% @end
job_update_properties(TargetId, Properties) ->
    gen_server:cast(?MASTER_CHANNEL,
        {job_update_properties, TargetId, Properties}).

%%----------------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%----------------------------------------------------------------------------
init([]) ->
    random:seed(erlang:now()),
    {ok, Read}  = application:get_env(monitor, master_chan_read_perm),
    {ok, Write} = application:get_env(monitor, master_chan_write_perm),
    {ok, Table} = init_database(),
    %{ok, Targets} = load_targets_conf_from_file(ConfFile),
    {ok, _Targets} = load_targets_conf_from_dets(Table),
    {ok, 
        #state{
            perm = #perm_conf{read=Read,write=Write},
            dets_ref = Table
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
%% SUPERCAST_CHANNEL BEHAVIOUR CALLS
%%----------------------------------------------------------------------------
handle_call({id_used, Id}, _F, #state{dets_ref=DetsRef} = S) ->
    Targets = get_all_targets(DetsRef),
    TargetAtomList  = [Tid    || #target{id=Tid} <- Targets],
    Probes          = [Probes || #target{probes=Probes} <- Targets],
    ProbeAtomList   = [PrId   || #probe{name=PrId} <- lists:flatten(Probes)],
    TotalAtomList   = lists:append(TargetAtomList, ProbeAtomList),
    Rep = lists:member(Id, TotalAtomList),
    {reply, Rep, S};

handle_call(get_perms, _F, #state{perm = P} = S) ->
    {reply, P, S};

handle_call({create_probe, TargetId, Probe}, _F, #state{dets_ref=DetsRef}=S) ->
    % is targetId a valid atom?
    %dets:sync(DetsRef),
    case (catch erlang:list_to_existing_atom(TargetId)) of
        {'EXIT', _} ->
            Msg = lists:flatten(
                io_lib:format("Target ~s did not exist",[TargetId])
            ),
            {reply, {error, Msg}, S};
        TargetAtom ->
            case dets:lookup(DetsRef, TargetAtom) of
                {error, Reason} ->
                    {reply, {error, Reason}, S};
                [TargetRecord] ->
                    {NProbe, NTarget} = insert_probe(Probe, TargetRecord),
                    ProbeInfoPdu = pdu(infoProbe, {create, TargetAtom, NProbe}),
                    Perm = NProbe#probe.permissions,
                    supercast_channel:emit(?MASTER_CHANNEL, {Perm, ProbeInfoPdu}),
                    dets:insert(DetsRef, NTarget),
                    monitor_data:write_probe(Probe),
                    {reply, ok, S};
                _ ->
                    {reply, {error, "Key error"}, S}
            end
    end;

handle_call({create_target, Target}, _F, #state{dets_ref = DetsRef} = S) ->
    % TODO check if id exist
    Target2 = load_target_conf(Target),
    %emit_wide(Target2),
    dets:insert(DetsRef, Target2),
    monitor_data:write_target(Target2),
    {reply, ok, S};

handle_call(dump_dets, _F, #state{dets_ref=DetsRef} = S) ->
    TargetsConf = get_all_targets(DetsRef),
    io:format("dump_dets: ~p~n",[TargetsConf]),
    {reply, {DetsRef, TargetsConf}, S}.

get_all_targets(Table) ->
    dets:foldr(fun(X, Acc) ->
        [X|Acc]
    end, [], Table).

insert_probe(Probe, Target) ->
    monitor_probe_sup:new({Target, Probe}),
    Probes = Target#target.probes,
    T2 = Target#target{probes = [Probe|Probes]},
    {Probe, T2}.

% emit_wide(Target) ->
%     Perm        = Target#target.global_perm,
%     PduTarget   = pdu(infoTarget, Target),
%     supercast_channel:emit(?MASTER_CHANNEL, {Perm, PduTarget}),
% 
%     TId         = Target#target.id,
%     Probes      = Target#target.probes,
%     PduProbes   = [{ProbePerm, pdu(infoProbe, {create, TId, Probe})} || 
%         #probe{permissions = ProbePerm} = Probe <- Probes],
%     lists:foreach(fun(X) ->
%         supercast_channel:emit(?MASTER_CHANNEL, X)
%     end, PduProbes).


%%----------------------------------------------------------------------------
%% SUPERCAST_CHANNEL BEHAVIOUR CASTS
%%----------------------------------------------------------------------------
handle_cast({sync_request, CState}, #state{dets_ref=DetsRef} = S) ->
    % TODO beter use dets:match to directly filter targets for user perms
    Targets = get_all_targets(DetsRef),
    % I want this client to receive my messages,
    supercast_channel:subscribe(?MASTER_CHANNEL, CState),
    % gen_dump_pdus will filter based on CState permissions
    {Pdus, Probes} = gen_dump_pdus(CState, Targets),
    supercast_channel:unicast(CState, Pdus),
    lists:foreach(fun(X) ->
        monitor_probe:triggered_return(X, CState)
    end, Probes),
    {noreply, S};

%%----------------------------------------------------------------------------
%% SELF API CASTS
%%----------------------------------------------------------------------------
handle_cast({probe_info, TargetId, NewProbe}, #state{dets_ref=DetsRef} = S) ->
    case dets:lookup(DetsRef, TargetId) of
        [] ->
            {noreply, S};
        [Target] ->
            Perms = NewProbe#probe.permissions,
            {ok, NewTarg} = update_target_from_probe_info(Target, NewProbe),
            dets:insert(DetsRef, NewTarg),
            Pdu = pdu(infoProbe, {update, TargetId, NewProbe}),
            supercast_channel:emit(?MASTER_CHANNEL, {Perms, Pdu}),
            {noreply, S}
    end;

handle_cast({job_update_properties, TargetId, NewProp}, #state{dets_ref=DetsRef} = S) ->
    case dets:lookup(DetsRef, TargetId) of
        [] ->
            nothing;
        [Target] ->
            TargetProp  = Target#target.properties,
            TargetProp2 = merge_properties(TargetProp, NewProp),
            case TargetProp2 of
                TargetProp ->
                    nothing;
                _ ->
                    % update dets
                    NewTarget = Target#target{properties=TargetProp2},
                    dets:insert(DetsRef, NewTarget),
                    %  update client side
                    PduTarget = pdu(infoTarget, NewTarget),
                    Perm = NewTarget#target.global_perm,
                    supercast_channel:emit(?MASTER_CHANNEL, {Perm, PduTarget})
            end
    end,
    {noreply, S};

handle_cast(_R, S) ->
    {noreply, S}.

handle_info(_, S) ->
    {noreply, S}.

terminate(_R, #state{dets_ref = DetsRef}) ->
    dets:close(DetsRef),
    normal.

code_change(_O, S, _E) ->
    {ok, S}.


%%----------------------------------------------------------------------------
%% UTILS    
%%----------------------------------------------------------------------------
merge_properties(Props, []) -> Props;
merge_properties(Props, [NewProp|NewProps]) ->
    {Key, _} = NewProp,
    Props2 = lists:keyreplace(Key, 1, Props, NewProp),
    merge_properties(Props2, NewProps).

update_target_from_probe_info(Target, Probe) ->
    TProperties     = Target#target.properties,
    PProperties     = Probe#probe.properties,
    PForward        = Probe#probe.forward_properties,

    {ok, NewTProp} = 
        probe_property_forward(TProperties, PProperties, PForward),


    Probes  = Target#target.probes,
    PrId    = Probe#probe.name,
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
            Pdu         = pdu(infoTarget, NTarget),
            supercast_channel:emit(?MASTER_CHANNEL, {TargetPerm, Pdu})
    end,
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
%    {ok, TargetsConf}  = file:consult(TargetsConfFile),
%    TargetsState = [],
%    load_targets_conf(TargetsConf, TargetsState).

load_targets_conf([], TargetsState) ->
    {ok, TargetsState};
load_targets_conf([T|Targets], TargetsState) ->
    T2 = load_target_conf(T),
    load_targets_conf(Targets, [T2|TargetsState]).
    
load_target_conf(Target) ->
    Dir = proplists:get_value(var_directory, Target#target.sys_properties),
    ok  = init_target_dir(Dir),
    ok  = init_target_snmp_conf(Target),
    ok  = init_target_jobs(Target#target.jobs),
    {ok, Target2}   = init_probes(Target),
    Target2.

init_target_snmp_conf(Target) ->
    SysProp = Target#target.sys_properties,
    case proplists:get_value(snmp_version, SysProp) of
        undefined   -> ok;
        SnmpVersion ->
            SnmpPort        = proplists:get_value(snmp_port,        SysProp),
            SnmpSecLevel    = proplists:get_value(snmp_seclevel,    SysProp),
            SnmpCommunity   = proplists:get_value(snmp_community,   SysProp),
            SnmpUsmUser     = proplists:get_value(snmp_usm_user,    SysProp),
            SnmpAuthKey     = proplists:get_value(snmp_authkey,     SysProp),
            SnmpAuthProto   = proplists:get_value(snmp_authproto,   SysProp),
            SnmpPrivKey     = proplists:get_value(snmp_privkey,     SysProp),
            SnmpPrivProto   = proplists:get_value(snmp_privproto,   SysProp),
            SnmpTimeout     = proplists:get_value(snmp_timeout,     SysProp),
            SnmpRetries     = proplists:get_value(snmp_retries,     SysProp),

            Props = Target#target.properties,
            IpVersion = proplists:get_value("ipVersion", Props),
            Ip        = proplists:get_value("ip",        Props),

            SnmpArgs = [
                {ip_address,        Ip},
                {ip_version,        IpVersion},
                {timeout,           SnmpTimeout},
                {port,              SnmpPort},
                {snmp_version,      SnmpVersion},
                {security_level,    SnmpSecLevel},
                {community,         SnmpCommunity},
                {auth_key,          SnmpAuthKey},
                {auth_proto,        SnmpAuthProto},
                {priv_key,          SnmpPrivKey},
                {priv_proto,        SnmpPrivProto},
                {retries,           SnmpRetries},
                {security_name,     SnmpUsmUser}
            ],

            AgentName = atom_to_list(Target#target.id),
            snmpman:register_element(AgentName, SnmpArgs)
    end.

init_target_jobs([]) -> ok;
init_target_jobs(
        [#job{name=Name,trigger=Trigger,module=M,function=F,argument=A}|Jobs]
            ) ->
    ok = equartz:register_internal_job(Name,Trigger,{M,F,atom_to_list(A)}),
    init_target_jobs(Jobs).

init_probes(Target) ->
    ProbesOrig  = Target#target.probes,
    ProbesNew   = [],
    init_probes(Target, ProbesOrig, ProbesNew).
init_probes(Target, [], ProbesNew) ->
    TargetNew = Target#target{probes = ProbesNew},
    {ok, TargetNew};
init_probes(Target, [P|Probes], ProbesN) ->
    monitor_probe_sup:new({Target, P}),
    ProbesN2  = [P|ProbesN],
    init_probes(Target, Probes, ProbesN2).

init_target_dir(Dir) ->
    case file:read_file_info(Dir) of
        {ok, _} ->
            ok;
        {error, enoent} ->
            file:make_dir(Dir);
        Other ->
            {error, Other}
    end.

%%----------------------------------------------------------------------------
%% PDU BUILD
%%----------------------------------------------------------------------------
pdu(infoTarget, #target{id=Id, properties=Prop}) ->
    AsnProps = lists:foldl(fun({K,V}, Acc) -> 
        [{'Property', K, V} | Acc]
    end, [], Prop),
    {modMonitorPDU,
        {fromServer,
            {infoTarget,
                {'InfoTarget',
                    atom_to_list(Id),
                    AsnProps,
                    [],
                    create}}}};

pdu(infoTargetUpdate, #target{id=Id, properties=Prop}) ->
    AsnProps = lists:foldl(fun({K,V}, Acc) ->
        [{'Property', K, V} | Acc]
    end, [], Prop),
    {modMonitorPDU,
        {fromServer,
            {infoTarget,
                {'InfoTarget',
                    atom_to_list(Id),
                    AsnProps,
                    [],
                    update}}}};

pdu(infoTargetDelete, Id) ->
    {modMonitorPDU,
        {fromServer,
            {infoTarget,
                {'InfoTarget',
                    atom_to_list(Id),
                    [],
                    delete}}}};

pdu(infoProbe, {InfoType, TargetId, 
        #probe{
            permissions         = #perm_conf{read = R, write = W},
            monitor_probe_conf  = ProbeConf,
            description         = Descr,
            info                = Info
        } = Probe
    }) ->
    P = {modMonitorPDU,
        {fromServer,
            {infoProbe,
                {'InfoProbe',
                    atom_to_list(TargetId),
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
    P.

gen_asn_probe_active(true)  -> 1;
gen_asn_probe_active(false) -> 0.

gen_asn_probe_conf(Conf) when is_record(Conf, nchecks_probe_conf) ->
    lists:flatten(io_lib:format("~p", [Conf]));
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

gen_logger_pdu({logger, bmonitor_logger_rrd2, Cfg}) ->
    Type = proplists:get_value(type, Cfg),
    RCreate = proplists:get_value(rrd_create, Cfg),
    RUpdate = proplists:get_value(rrd_update, Cfg),
    RGraphs = proplists:get_value(rrd_graph, Cfg),
    Indexes = [I || {I,_} <- proplists:get_value(row_index_to_rrd_file, Cfg)],
    {loggerRrd2, 
        {'LoggerRrd2',
            atom_to_list(bmonitor_logger_rrd2),
            atom_to_list(Type),
            RCreate,
            RUpdate,
            RGraphs,
            Indexes
        }
    };

gen_logger_pdu({logger, bmonitor_logger_text, Cfg}) ->
    {loggerText, 
        {'LoggerText', 
            atom_to_list(bmonitor_logger_text), 
            to_string(Cfg)}}.

gen_dump_pdus(CState, Targets) ->
    FTargets    = supercast:filter(CState, [{Perm, Target} ||
        #target{global_perm = Perm} = Target <- Targets]),
    TargetsPDUs = [pdu(infoTarget, Target) || Target <- FTargets],
    ProbesDefs  = [{TId, Probes} ||
        #target{id = TId, probes = Probes} <- FTargets],
    gen_dump_pdus(CState, TargetsPDUs, [], [], ProbesDefs).
gen_dump_pdus(_, TargetsPDUs, ProbesPDUs, ProbesFiltered, []) ->
    {lists:append(TargetsPDUs, ProbesPDUs), ProbesFiltered};
gen_dump_pdus(CState, TargetsPDUs, ProbesPDUs, PFList, [{TId, Probes}|T]) ->
    ProbesThings  = [{Perm, Probe} || 
        #probe{permissions = Perm} = Probe <- Probes],
    AllowedThings = supercast:filter(CState, ProbesThings),
    PFListN = [PName || #probe{name = PName} <- Probes],
    Result = [pdu(infoProbe, {create, TId, Probe}) ||
        Probe <- AllowedThings],
    gen_dump_pdus(
        CState,
        TargetsPDUs,
        lists:append(ProbesPDUs, Result),
        lists:append(PFListN,PFList),
        T).

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

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

% API
-export([
    start_link/0,

    probe_info/2,

    create_target/1,
    create_probe/1,

    %job_update_properties/2,

    generate_name/1,
    init_target/1
]).

-record(state, {
    perm
}).

% generate id range
% 1 000 000 possible values
-define(RAND_RANGE, 1000000).
% but must be a minimum of 100000
-define(RAND_MIN,   99999).

-define(DETS_TARGETS, 'targets_db').


-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link(
      {via, supercast_registrar, {?MODULE, ?MASTER_CHANNEL}},
      ?MODULE, [], []).

%% monitor_commander API
-spec create_target(Target::#target{}) -> ok | {error, Info::string()}.
% @doc
% Called from monitor_commander.
% @end
create_target(Target) ->
    gen_server:call({via, supercast_registrar, ?MASTER_CHANNEL}, {create_target, Target}).

-spec create_probe(Probe::#probe{}) 
    -> ok | {error, string()}.
create_probe(Probe) ->
    gen_server:call({via, supercast_registrar, ?MASTER_CHANNEL}, {create_probe, Probe}).

-spec generate_name(Head::string()) -> atom().
% @doc
% Called from monitor_commander.
% @end
generate_name(Head) ->
    Int         = random:uniform(?RAND_RANGE),
    RandId      = Int + ?RAND_MIN,
    RandIdL     = io_lib:format("~p", [RandId]),
    RandIdS     = lists:flatten(RandIdL),
    Name        = lists:concat([Head, RandIdS]),
    Name.
    %case gen_server:call({via, supercast_registrar, ?MASTER_CHANNEL}, {name_used, Name}) of
        %false->  {ok, Name};
        %true  -> generate_name(Head)
    %end.

%%----------------------------------------------------------------------------
%% supercast_channel API
%%----------------------------------------------------------------------------
-spec get_perms(PidName::atom()) -> {ok, PermConf::#perm_conf{}}.
get_perms(PidName) ->
    gen_server:call({via, supercast_registrar, PidName}, get_perms).

-spec sync_request(PidName::atom(), CState::tuple()) ->  ok.
sync_request(PidName, CState) ->
    gen_server:cast({via, supercast_registrar, PidName}, {sync_request, CState}).


%%----------------------------------------------------------------------------
%% monitor_probe API
%%----------------------------------------------------------------------------
-spec probe_info(TargetName::string(), Probe::#probe{}) -> ok.
% @doc
% Called by a monitor_probe when information must be forwarded
% to subscribers of ?MASTER_CHANNEL concerning the probe.
% @end
probe_info(TargetName, Probe) ->
    gen_server:cast({via, supercast_registrar, ?MASTER_CHANNEL}, {probe_info, TargetName, Probe}).

%%----------------------------------------------------------------------------
%% monitor_jobs API    
%%----------------------------------------------------------------------------
%-spec job_update_properties(TargetId::atom(), Properties::[{string(),any()}]) ->
%    ok.
% @doc
% Called by various monitor_jobs functions to update target properties.
% @end
%job_update_properties(TargetId, Properties) ->
%    gen_server:cast({via, supercast_registrar, ?MASTER_CHANNEL},
%        {job_update_properties, TargetId, Properties}).

%%----------------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%----------------------------------------------------------------------------
init([]) ->
    random:seed(erlang:now()),
    {ok, Read}  = application:get_env(monitor, master_chan_read_perm),
    {ok, Write} = application:get_env(monitor, master_chan_write_perm),
    %{ok, Table} = init_database(),
    %{ok, Targets} = load_targets_conf_from_file(ConfFile),
    %{ok, _Targets} = load_targets_conf_from_dets(Table),
    {atomic, _} = init_targets(),
    {atomic, _} = init_probes(),
    {atomic, _} = init_jobs(),
    {ok, 
        #state{
            perm = #perm_conf{read=Read,write=Write}
        }
    }.
    
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

% init_database() ->
%     {ok, VarDir} = application:get_env(monitor, targets_data_dir),
%     DetsFile     = filename:absname_join(VarDir, "targets.dets"),
%     case filelib:is_file(DetsFile) of
%         true  ->
%             case dets:is_dets_file(DetsFile) of
%                 true ->
%                     dets:open_file(DetsFile);
%                 false ->
%                     ok = file:delete(DetsFile),
%                     dets:open_file(DetsFile)
%             end;
%         false ->
%             {ok, N} = dets:open_file(?DETS_TARGETS, [
%                 {file,   DetsFile},
%                 {keypos, 2},
%                 {ram_file, false},
%                 {auto_save, 180000},
%                 {type, set}
%             ]),
%             dets:close(N),
%             dets:open_file(DetsFile)
%     end.

%%----------------------------------------------------------------------------
%% SUPERCAST_CHANNEL BEHAVIOUR CALLS
%%----------------------------------------------------------------------------
handle_call(get_perms, _F, #state{perm = P} = S) ->
    {reply, P, S};

%%----------------------------------------------------------------------------
%% OTHER CALLS
%%----------------------------------------------------------------------------
%handle_call({name_used, Name}, _F, #state{dets_ref=DetsRef} = S) ->
    %Targets = get_all_targets(DetsRef),
    %TargetNameList  = [TargetName    || #target{name=TargetName} <- Targets],
    %Probes          = [Probes || #target{probes=Probes} <- Targets],
    %ProbeAtomList   = [PrId   || #probe{name=PrId} <- lists:flatten(Probes)],
    %TotalList       = lists:append(TargetNameList, ProbeAtomList),
    %Rep = lists:member(Name, TotalList),
    %{reply, Rep, S};


handle_call({create_probe, Probe}, _F, S) ->
    monitor_probe_sup:new(Probe),
    monitor_data:write_probe(Probe),
    {reply, ok, S};

handle_call({create_target, Target}, _F, S) ->
    % TODO check if id exist
    init_target(Target),
    %emit_wide(Target2),
    %dets:insert(DetsRef, Target2),
    monitor_data:write_target(Target),
    {reply, ok, S}.

%%----------------------------------------------------------------------------
%% SUPERCAST_CHANNEL BEHAVIOUR CASTS
%%----------------------------------------------------------------------------
handle_cast({sync_request, CState}, S) ->
    % TODO beter use dets:match to directly filter targets for user perms
    %Targets = get_all_targets(DetsRef),
    % I want this client to receive my messages,
    supercast_channel:subscribe(?MASTER_CHANNEL, CState),
    % gen_dump_pdus will filter based on CState permissions
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

%%----------------------------------------------------------------------------
%% SELF API CASTS
%%----------------------------------------------------------------------------
handle_cast({probe_info, _TargetName, NewProbe}, S) ->
    io:format("probe info?"),
    monitor_data:write_probe(NewProbe),
    {noreply, S};
    %case dets:lookup(DetsRef, TargetName) of
        %[] ->
            %{noreply, S};
        %[Target] ->
            %Perms = NewProbe#probe.permissions,
            %{ok, NewTarg} = update_target_from_probe_info(Target, NewProbe),
            %dets:insert(DetsRef, NewTarg),
            %Pdu = pdu(infoProbe, {update, TargetName, NewProbe}),
            %supercast_channel:emit(?MASTER_CHANNEL, {Perms, Pdu}),
    %end;

%handle_cast({job_update_properties, TargetId, NewProp}, S) ->
    %case dets:lookup(DetsRef, TargetId) of
        %[] ->
            %nothing;
        %[Target] ->
            %TargetProp  = Target#target.properties,
            %TargetProp2 = merge_properties(TargetProp, NewProp),
            %case TargetProp2 of
                %TargetProp ->
                    %nothing;
                %_ ->
                    %% update dets
                    %NewTarget = Target#target{properties=TargetProp2},
                    %dets:insert(DetsRef, NewTarget),
                    %%  update client side
                    %PduTarget = pdu(infoTarget, NewTarget),
                    %Perm = NewTarget#target.global_perm,
                    %supercast_channel:emit(?MASTER_CHANNEL, {Perm, PduTarget})
            %end
    %end,
    %{noreply, S};

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
%merge_properties(Props, []) -> Props;
%merge_properties(Props, [NewProp|NewProps]) ->
    %{Key, _} = NewProp,
    %Props2 = lists:keyreplace(Key, 1, Props, NewProp),
    %merge_properties(Props2, NewProps).

% update_target_from_probe_info(Target, Probe) ->
%     TProperties     = Target#target.properties,
%     PProperties     = Probe#probe.properties,
%     PForward        = Probe#probe.forward_properties,
% 
%     {ok, NewTProp} = 
%         probe_property_forward(TProperties, PProperties, PForward),
% 
% 
%     Probes  = Target#target.probes,
%     PrId    = Probe#probe.name,
%     NProbes = lists:keystore(PrId, 2, Probes, Probe),
%     NTarget = Target#target{
%         probes      = NProbes,
%         properties  = NewTProp
%     },
%     case NewTProp of
%         TProperties ->
%             % nothing to do
%             ok;
%         _ ->
%             % update target properties state to the client
%             TargetPerm  = NTarget#target.global_perm,
%             Pdu         = pdu(infoTarget, NTarget),
%             supercast_channel:emit(?MASTER_CHANNEL, {TargetPerm, Pdu})
%     end,
%     {ok, NTarget}.

% probe_property_forward(TProperties, _, []) ->
%     {ok, TProperties};
% probe_property_forward(TProperties, PProperties, [P|Rest]) ->
%     case lists:keyfind(P, 1, PProperties) of
%         false ->
%             probe_property_forward(TProperties, PProperties, Rest);
%         {P, Val} ->
%             NTProperties = lists:keystore(P,1,TProperties, {P,Val}),
%             probe_property_forward(NTProperties, PProperties, Rest)
%     end.

    

%load_targets_conf_from_dets(Table) ->
%    TargetsConf = dets:foldr(fun(X, Acc) ->
%        [X|Acc]
%    end, [], Table),
%    TargetsState = [],
%    load_targets_conf(TargetsConf, TargetsState).

%load_targets_conf_from_file(TargetsConfFile) ->
%    {ok, TargetsConf}  = file:consult(TargetsConfFile),
%    TargetsState = [],
%    load_targets_conf(TargetsConf, TargetsState).

%load_targets_conf([], TargetsState) ->
    %{ok, TargetsState};
%load_targets_conf([T|Targets], TargetsState) ->
    %T2 = load_target_conf(T),
    %load_targets_conf(Targets, [T2|TargetsState]).
    
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
    %ok  = init_target_jobs(Target#target.jobs).

%init_target_jobs([]) -> ok;
%init_target_jobs(
        %[#job{name=Name,trigger=Trigger,module=M,function=F,argument=A}|Jobs]
            %) ->
    %ok = equartz:register_internal_job(Name,Trigger,{M,F,A}),
    %init_target_jobs(Jobs).

%init_probes(Target) ->
    %ProbesOrig  = Target#target.probes,
    %ProbesNew   = [],
    %init_probes(Target, ProbesOrig, ProbesNew).
%init_probes(Target, [], ProbesNew) ->
    %TargetNew = Target#target{probes = ProbesNew},
    %{ok, TargetNew};
%init_probes(Target, [P|Probes], ProbesN) ->
    %monitor_probe_sup:new({Target, P}),
    %ProbesN2  = [P|ProbesN],
    %init_probes(Target, Probes, ProbesN2).

    %%----------------------------------------------------------------------------
%% PDU BUILD
%%----------------------------------------------------------------------------
% pdu(infoTarget, #target{name=Name, properties=Prop}) ->
%     AsnProps = lists:foldl(fun({K,V}, Acc) -> 
%         [{'Property', K, V} | Acc]
%     end, [], Prop),
%     {modMonitorPDU,
%         {fromServer,
%             {infoTarget,
%                 {'InfoTarget',
%                     Name,
%                     AsnProps,
%                     [],
%                     create}}}};
% 
% pdu(infoTargetUpdate, #target{name=Name, properties=Prop}) ->
%     AsnProps = lists:foldl(fun({K,V}, Acc) ->
%         [{'Property', K, V} | Acc]
%     end, [], Prop),
%     {modMonitorPDU,
%         {fromServer,
%             {infoTarget,
%                 {'InfoTarget',
%                     Name,
%                     AsnProps,
%                     [],
%                     update}}}};
% 
% pdu(infoTargetDelete, Id) ->
%     {modMonitorPDU,
%         {fromServer,
%             {infoTarget,
%                 {'InfoTarget',
%                     Id,
%                     [],
%                     delete}}}};
% 
% pdu(infoProbe, {InfoType, TargetName, 
%         #probe{
%             permissions         = #perm_conf{read = R, write = W},
%             monitor_probe_conf  = ProbeConf,
%             description         = Descr,
%             info                = Info
%         } = Probe
%     }) ->
%     P = {modMonitorPDU,
%         {fromServer,
%             {infoProbe,
%                 {'InfoProbe',
%                     TargetName,
%                     Probe#probe.name,
%                     Descr,
%                     Info,
%                     {'PermConf', R, W},
%                     atom_to_list(Probe#probe.monitor_probe_mod),
%                     gen_asn_probe_conf(ProbeConf),
%                     Probe#probe.status,
%                     Probe#probe.timeout,
%                     Probe#probe.step,
%                     gen_asn_probe_inspectors(Probe#probe.inspectors),
%                     gen_asn_probe_loggers(Probe#probe.loggers),
%                     gen_asn_probe_properties(Probe#probe.properties),
%                     gen_asn_probe_active(Probe#probe.active),
%                     InfoType}}}},
%     P.

% gen_asn_probe_active(true)  -> 1;
% gen_asn_probe_active(false) -> 0.
% 
% gen_asn_probe_conf(Conf) when is_record(Conf, nchecks_probe_conf) ->
%     lists:flatten(io_lib:format("~p", [Conf]));
% gen_asn_probe_conf(Conf) when is_record(Conf, snmp_probe_conf) ->
%     lists:flatten(io_lib:format("~p", [Conf])).
% 
% gen_asn_probe_inspectors(Inspectors) ->
%     [{
%         'Inspector',
%         atom_to_list(Module),
%         lists:flatten(io_lib:format("~p", [Conf]))
%     } || {_, Module, Conf} <- Inspectors].
% 
% gen_asn_probe_loggers(Loggers) ->
%     [gen_logger_pdu(LConf) || LConf <- Loggers].
% 
% gen_logger_pdu({logger, bmonitor_logger_rrd2, Cfg}) ->
%     Type = proplists:get_value(type, Cfg),
%     RCreate = proplists:get_value(rrd_create, Cfg),
%     RUpdate = proplists:get_value(rrd_update, Cfg),
%     RGraphs = proplists:get_value(rrd_graph, Cfg),
%     Indexes = [I || {I,_} <- proplists:get_value(row_index_to_rrd_file, Cfg)],
%     {loggerRrd2, 
%         {'LoggerRrd2',
%             atom_to_list(bmonitor_logger_rrd2),
%             atom_to_list(Type),
%             RCreate,
%             RUpdate,
%             RGraphs,
%             Indexes
%         }
%     };
% 
% gen_logger_pdu({logger, bmonitor_logger_text, Cfg}) ->
%     {loggerText, 
%         {'LoggerText', 
%             atom_to_list(bmonitor_logger_text), 
%             to_string(Cfg)}}.

% gen_dump_pdus(CState, Targets) ->
%     FTargets    = supercast:filter(CState, [{Perm, Target} ||
%         #target{global_perm = Perm} = Target <- Targets]),
%     TargetsPDUs = [pdu(infoTarget, Target) || Target <- FTargets],
%     ProbesDefs  = [{Name, Probes} ||
%         #target{name=Name, probes = Probes} <- FTargets],
%     gen_dump_pdus(CState, TargetsPDUs, [], [], ProbesDefs).
% gen_dump_pdus(_, TargetsPDUs, ProbesPDUs, ProbesFiltered, []) ->
%     {lists:append(TargetsPDUs, ProbesPDUs), ProbesFiltered};
% gen_dump_pdus(CState, TargetsPDUs, ProbesPDUs, PFList, [{TId, Probes}|T]) ->
%     ProbesThings  = [{Perm, Probe} || 
%         #probe{permissions = Perm} = Probe <- Probes],
%     AllowedThings = supercast:filter(CState, ProbesThings),
%     PFListN = [PName || #probe{name = PName} <- Probes],
%     Result = [pdu(infoProbe, {create, TId, Probe}) ||
%         Probe <- AllowedThings],
%     gen_dump_pdus(
%         CState,
%         TargetsPDUs,
%         lists:append(ProbesPDUs, Result),
%         lists:append(PFListN,PFList),
%         T).

% to_string(Term) ->
%     lists:flatten(io_lib:format("~p", [Term])).
% 
% gen_asn_probe_properties(K) ->
%     gen_asn_probe_properties(K, []).
% gen_asn_probe_properties([], S) ->
%     S;
% gen_asn_probe_properties([{K,V} | T], S) when is_list(V) ->
%     gen_asn_probe_properties(T, [{'Property', K, V} | S]);
% gen_asn_probe_properties([{K,V} | T], S) when is_integer(V) ->
%     gen_asn_probe_properties(T, [{'Property', K, integer_to_list(V)} | S]);
% gen_asn_probe_properties([{K,V} | T], S) when is_float(V) ->
%     gen_asn_probe_properties(T, [{'Property', K, float_to_list(V, [{decimals, 10}])} | S]);
% gen_asn_probe_properties([{K,V} | T], S) when is_atom(V) ->
%     gen_asn_probe_properties(T, [{'Property', K, atom_to_list(V)} | S]).

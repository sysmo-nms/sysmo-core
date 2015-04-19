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
% @doc
% @end
-module(probe_simple_nchecks).
-behaviour(gen_server).
-behaviour(supercast_channel).
-include("include/monitor.hrl").
-include("../nchecks/include/nchecks.hrl").
-include_lib("xmerl/include/xmerl.hrl").


% gen_server
-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).
-export([get_perms/1,sync_request/2]).
-export([exec_nchecks/3]).

-record(state, {name,ref}).
-record(nchecks_state, {
    class,
    args,
    dump_dir,
    rrd_config,
    opaque = <<>>
}).





start_link(#probe{name=Name} = Probe) ->
    gen_server:start_link({via, supercast_registrar, {?MODULE, Name}}, ?MODULE, Probe, []).

do_init(Probe) ->
    Ref = make_ref(),
    random:seed(erlang:now()),
    {ok, DumpDir}   = application:get_env(supercast, http_sync_dir),
    {Class, Args}   = init_nchecks(Probe),
    RrdConfig       = rrd4j_init(Probe),
    TRef            = monitor:send_after_rand(Probe#probe.step, {take_of, Ref}),
    NS = #nchecks_state{
        class = Class,
        args = Args,
        dump_dir = DumpDir,
        rrd_config = RrdConfig
    },
    ES = #ets_state{
        name             = Probe#probe.name,
        permissions      = Probe#probe.permissions,
        belong_to        = Probe#probe.belong_to,
        tref             = TRef,
        current_status_from = erlang:now(),
        current_status   = Probe#probe.status,
        local_state = NS
    },

    monitor_data_master:set_probe_state(ES),

    % partial return for clients allready connected
    PartialReturn = #probe_return{status=ES#ets_state.current_status},
    MilliRem = monitor:read_timer(ES#ets_state.tref),
    Pdu = monitor_pdu:probeReturn(
        PartialReturn,
        ES#ets_state.belong_to,   
        ES#ets_state.name,
        MilliRem
    ),
    supercast_channel:emit(?MASTER_CHANNEL, {ES#ets_state.permissions, Pdu}),
    Ref.





%%----------------------------------------------------------------------------
%% supercast channel behaviour API
%%----------------------------------------------------------------------------
get_perms(PidName) ->
    #ets_state{permissions=Perm} = monitor_data_master:get_probe_state(PidName),
    Perm.

sync_request(PidName, CState) ->
    gen_server:cast({via, supercast_registrar, PidName}, {sync_request, CState}).

do_sync_request(CState, S) ->
    ES       = monitor_data_master:get_probe_state(S#state.name),
    % the rrd file
    {Type, RrdCfg}  = ES#ets_state.local_state#nchecks_state.rrd_config,
    case Type of
        simple ->
            RrdFile = RrdCfg,
            RrdFileBase = filename:basename(RrdFile),

            % generate tmp dir in dump dir
            DumpDir  = ES#ets_state.local_state#nchecks_state.dump_dir,
            TmpDir   = monitor:generate_temp_dir(),
            TmpPath  = filename:join(DumpDir, TmpDir),
            file:make_dir(TmpPath),

            % copy rrdfile to tmpdir
            DumpFile = filename:join(TmpPath,RrdFileBase),
            file:copy(RrdFile, DumpFile),

            % build the PDU
            Pdu = monitor_pdu:nchecksSimpleDumpMessage(S#state.name, TmpDir, RrdFileBase),
            supercast_channel:subscribe(ES#ets_state.name, CState),
            supercast_channel:unicast(CState, [Pdu]);
        table ->
            % TODO
            ok;
        _ ->
            ok
    end.
%%----------------------------------------------------------------------------
%% supercast channel behaviour API END
%%----------------------------------------------------------------------------





%%----------------------------------------------------------------------------
%% monitor API
%%----------------------------------------------------------------------------
do_trigger_return(CState, S) ->
    ES = monitor_data_master:get_probe_state(S#state.name),

    PartialPR = #probe_return{status=ES#ets_state.current_status},

    MilliRem = monitor:read_timer(ES#ets_state.tref),
    Pdu = monitor_pdu:probeReturn(
        PartialPR,
        ES#ets_state.belong_to,
        ES#ets_state.name,
        MilliRem
    ),
    supercast_channel:unicast(CState, [Pdu]),
    ok.

do_force(#state{ref=Ref} = S) ->
    ES  = monitor_data_master:get_probe_state(S#state.name),
    case erlang:cancel_timer(ES#ets_state.tref) of
        false ->
            ok;
        _ ->
            TRef = monitor:send_after(0, {take_of, Ref}),
            monitor_data_master:set_probe_state(ES#ets_state{tref=TRef}),
            PartialReturn = #probe_return{status=ES#ets_state.current_status},
            Pdu = monitor_pdu:probeReturn(
                PartialReturn,
                ES#ets_state.belong_to,
                ES#ets_state.name,
                500
            ),
            supercast_channel:emit(?MASTER_CHANNEL, {ES#ets_state.permissions, Pdu})
    end.
%%----------------------------------------------------------------------------
%% monitor API END
%%----------------------------------------------------------------------------





%%----------------------------------------------------------------------------
%% INTERNALS
%%----------------------------------------------------------------------------
do_handle_probe_return(PR, #state{ref=Ref} = S) ->
    % get the probe state
    ES  = monitor_data_master:get_probe_state(S#state.name),

    % set status and update monitor_events and data_master if required
    [Probe]  = monitor_data_master:get(probe, S#state.name),
    OldStatus = Probe#probe.status,
    NewStatus = PR#probe_return.status,
    case NewStatus of
        OldStatus ->
            monitor_events:notify(S#state.name, OldStatus);
        _ ->
            monitor_events:notify_move(S#state.name, NewStatus),
            NewProbe = Probe#probe{status = NewStatus},
            monitor_data_master:update(probe, NewProbe)
    end,

    % errd4j update
    {Type, RrdCfg} = ES#ets_state.local_state#nchecks_state.rrd_config,
    case Type of
        simple ->
            RrdFile = RrdCfg,
            [{_,Perfs}] = PR#probe_return.perfs,
            ok = errd4j:update(RrdFile, Perfs, PR#probe_return.timestamp),

            % SEND MESSAGE to subscribers of self() to update their rrds
            Pdu2 = monitor_pdu:nchecksSimpleUpdateMessage(S#state.name,PR#probe_return.timestamp,Perfs),
            supercast_channel:emit(S#state.name, {ES#ets_state.permissions, Pdu2});
        table ->
            % TODO
            ok;
        _ ->
            crap
    end,

    % initiate LAUNCH
    TRef        = monitor:send_after(Probe#probe.step, {take_of,Ref}),
    MilliRem    = monitor:read_timer(TRef),

    % SEND MESSAGES to subscribers of master channel
    Pdu = monitor_pdu:probeReturn(
        PR,
        ES#ets_state.belong_to,
        ES#ets_state.name,
        MilliRem),
    supercast_channel:emit(?MASTER_CHANNEL, {ES#ets_state.permissions, Pdu}),

    % WRITE
    monitor_data_master:set_probe_state(
        ES#ets_state{
            tref=TRef,
            current_status_from = erlang:now(),
            current_status=PR#probe_return.status
        }
    ).

do_take_of(#state{ref=Ref,name=PName}) ->
    ES      = monitor_data_master:get_probe_state(PName),
    Class   = ES#ets_state.local_state#nchecks_state.class,
    Args    = ES#ets_state.local_state#nchecks_state.args,
    Opaque  = ES#ets_state.local_state#nchecks_state.opaque,
    ToPid   = self(),
    erlang:spawn(fun() ->
        {ok, Return}  = ?MODULE:exec_nchecks(Class, Args, Opaque),
        erlang:send(ToPid, {probe_return, Ref, Return})
    end).
%%----------------------------------------------------------------------------
%% INTERNALS END
%%----------------------------------------------------------------------------





%%----------------------------------------------------------------------------
%% GEN_SERVER
%%----------------------------------------------------------------------------
init(Probe) ->
    % to let multiple probes initialize in the same time, init is delayed.
    gen_server:cast(self(), do_init),
    {ok, Probe}.


handle_cast(do_init, #probe{name=PName} = Probe) ->
    Ref = do_init(Probe),
    {noreply, #state{name=PName,ref=Ref}};

handle_cast({sync_request, CState}, S) ->
    do_sync_request(CState, S),
    {noreply, S};

handle_cast({trigger_return, CState}, S) ->
    do_trigger_return(CState, S),
    {noreply, S};

handle_cast(force, S) ->
    do_force(S),
    {noreply, S};

handle_cast(_Cast, S) ->
    {noreply, S}.


handle_call(shut_it_down, _F, #state{name=Name} = S) ->
    supercast_channel:delete(Name),
    {stop, shutdown, ok, S};

handle_call(_Call, _From, S) ->
    {noreply, S}.


handle_info({probe_return, Ref, PR}, #state{ref=Ref} = S) ->
    do_handle_probe_return(PR, S),
    {noreply, S};

handle_info({take_of, Ref}, #state{ref=Ref} = S) ->
    do_take_of(S),
    {noreply, S};

handle_info(_I, SData) ->
    {noreply, SData}.


terminate(_Reason, _S) ->
    normal.


code_change(_OldVsn, S, _Extra) ->
    {ok, S}.


%%
%% UTILS
%%
%emit_all(_, _, []) -> ok;
%emit_all(Name, Perm, [Pdu|T]) ->
%    supercast_channel:emit(Name,{Perm, Pdu}),
%    emit_all(Name,Perm,T).


%%----------------------------------------------------------------------------
%% Nchecks functions
%%----------------------------------------------------------------------------
init_nchecks(Probe) ->
    [Target]    = monitor_data_master:get(target, Probe#probe.belong_to),
    TargetProp  = Target#target.properties,
    Conf        = Probe#probe.module_config,
    #nchecks_probe_conf{class = Class, args = Args} = Conf,

    % if "host" is not defined in probe conf, use the target "host" property
    % TODO use Check.xml definition
    case proplists:lookup("host", Args) of
        none ->
            TargHost = proplists:lookup("host", TargetProp),
            NewArgs  = [TargHost|Args];
        _ ->
            NewArgs = Args
    end,
    {Class, NewArgs}.


exec_nchecks(Class, Args, Opaque) ->
    case nchecks:check(Class,Args,Opaque) of
        {ok, Reply} ->
            #nchecks_reply{
               status=Status,
                performances=Perfs,
                reply_string=Str,
                timestamp=Ts,
                opaque=Opaque
            } = Reply,
            ProbeReturn = #probe_return{
                status          = Status,
                reply_string    = Str,
                timestamp       = Ts,
                perfs           = Perfs,
                opaque          = Opaque
            };
        %{error, busy} ->       TODO (status UNKNOWN)
        %{error, no_worker} ->  TODO (status UNKNOWN)
        {error, Error} ->
            ProbeReturn = #probe_return{
                status          = "ERROR",
                reply_string    = Error,
                opaque          = Opaque
            }

    end,
    {ok, ProbeReturn}.

%%----------------------------------------------------------------------------
%% errd4j functions
%%----------------------------------------------------------------------------


rrd4j_init(#probe{name=Name,step=Step,belong_to=TargetName,module_config=NCheck}) ->

    % Get the target directory TargetDir
    [Target]    = monitor_data_master:get(target, TargetName),
    TargetDir   = proplists:get_value(var_directory, Target#target.sys_properties),

    % Generate check path dir and maybe create
    ProbeDir = filename:join([TargetDir, Name]),
    case filelib:is_dir(ProbeDir) of
        false -> file:make_dir(ProbeDir);
        true -> ok
    end,

    % Generate XML Class definition file path
    Class = NCheck#nchecks_probe_conf.class,
    ClassDefinitionFile = string:concat(Class, ".xml"),
    ClassDefinitionPath = filename:join(["cfg", "nchecks", ClassDefinitionFile]),
    
    % Load XML file content
    {#xmlDocument{content=XDocument_Content}, _} =
        xmerl_scan:file(ClassDefinitionPath, [{document, true}]),

    % Extract <NChecks> content
    #xmlElement{content=XNChecks_Content} =
        lists:keyfind('NChecks', 2, XDocument_Content),

    % Extract <Check> content
    #xmlElement{content=XCheck_Content} =
        lists:keyfind('Check', 2, XNChecks_Content),

    % Extract <Performances> content and attributes. This is where we
    % have all our relevant informations.
    #xmlElement{
        content=XPerformances_Content,
        attributes=XPerformances_Attrib
    } = lists:keyfind('Performances', 2, XCheck_Content),

    % Extract <Performances Type="?"> attributes and switch
    #xmlAttribute{value=XPerformances_Attr_Type} =
        lists:keyfind('Type', 2, XPerformances_Attrib),

    case XPerformances_Attr_Type of
        "table" ->
            % "table" Performance type mean one rrd file definition
            % for x files. For monitoring lists of things that return
            % values. For example, interfaces staticstics.
            % TODO
            {table, []};
        "simple" ->
            % "simple" Performance type mean only one rrd file (but off course
            % possibly multiple datasources)

            % Get the FileName
            #xmlAttribute{value=XPerformances_Attr_FileName} =
                lists:keyfind('FileName', 2, XPerformances_Attrib),

            % Generate rrd file path
            RrdFilePath = filename:join([ProbeDir, XPerformances_Attr_FileName]),

            case filelib:is_regular(RrdFilePath) of
                true ->
                    % Allready done
                    {simple, RrdFilePath};
                false ->

                    % Get the list of RRD datasources
                    #xmlElement{content=XDataSourceTable_Content} =
                        lists:keyfind('DataSourceTable', 2, XPerformances_Content),
            
                    % Filter and only keep xmlElements
                    XDataSourceTable_Elements = lists:filter(fun(E) ->
                        is_record(E, xmlElement)
                    end, XDataSourceTable_Content),

                    % Build DS definition tuples for errd4j
                    DSDefinitions = lists:map(fun(#xmlElement{attributes=XAttrib}) ->
                        #xmlAttribute{value=DsId}    = lists:keyfind('Id', 2, XAttrib),
                        #xmlAttribute{value=DsType}  = lists:keyfind('Type', 2, XAttrib),
                        {DsId, DsType, Step *2, 'Nan', 'Nan'}
                    end, XDataSourceTable_Elements),

                    % Create rrd file.
                    ok = errd4j:create(RrdFilePath, Step, DSDefinitions, "default"),

                    % Return rrd file path
                    {simple, RrdFilePath}
            end;
        _ ->
            error
    end.


% rrd4j_init2(#probe{name=Name,step=Step,belong_to=TargetName,module_config=NCheck}) ->
% 
%     % get the target directory TargetDir
%     [Target]    = monitor_data_master:get(target, TargetName),
%     TargetDir   = proplists:get_value(var_directory, Target#target.sys_properties),
% 
%     % generate rrd file path
%     ProbeFile       = string:concat(Name, ".rrd"),
%     ProbeFilePath   = filename:join([TargetDir, ProbeFile]),
% 
%     % does the rrd file allready exists?
%     case filelib:is_regular(ProbeFilePath) of
%         true ->
%             % only return the processed filepath
%             ProbeFilePath;
%         false ->
%             % we will create the rrd defined in the nchecks class xml file
%             Class           = NCheck#nchecks_probe_conf.class,
%             ClassFile       = string:concat(Class, ".xml"),
%             ClassFilePath   = filename:join(["cfg", "nchecks", ClassFile]),
% 
%             % extract XML file content
%             {#xmlDocument{content=Document_Content}, _} = xmerl_scan:file(ClassFilePath, [{document, true}]),
%             % extract <NChecks>
%             #xmlElement{content=NChecks_Content} = lists:keyfind('NChecks', 2, Document_Content),
%             % extract <Check>
%             #xmlElement{content=Check_Content} = lists:keyfind('Check', 2, NChecks_Content),
%             % extract <Performances> content
%             #xmlElement{
%                 content=Performances_Content,
%                 attributes=Performances_Attrib
%             } = lists:keyfind('Performances', 2, Check_Content),
%             % <Performances Type="simple" | "table">
%             #xmlAttribute{value=_Type} = lists:keyfind('Type', 2, Performances_Attrib),
%             % TODO if Type = "table" chech attr TableFlag -> module_config -> Args -> Flag
%             #xmlElement{
%                 content=DataSourceTable_Content
%             } = lists:keyfind('DataSourceTable', 2, Performances_Content),
%             
%             % only keep xmlElements
%             DataSourceTableElements = lists:filter(fun(E) ->
%                 is_record(E, xmlElement)
%             end, DataSourceTable_Content),
% 
%             % build DS definition tuples
%             DataSourceDefinitions = lists:map(fun(#xmlElement{attributes=Attrib}) ->
%                 #xmlAttribute{value=DsId}    = lists:keyfind('Id', 2, Attrib),
%                 #xmlAttribute{value=DsType}  = lists:keyfind('Type', 2, Attrib),
%                 {DsId, DsType, Step *2, 'Nan', 'Nan'}
%             end, DataSourceTableElements),
% 
%             % create rrd file
%             ok = errd4j:create(ProbeFilePath, Step, DataSourceDefinitions, "default"),
% 
%             % return the processed filepath
%             ProbeFilePath
%     end.

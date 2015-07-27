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
% This is the main and most complex module of sysmo. To work correctly,
% Nchecks definitions xml file must be accessible and valid.
% @end
-module(nchecks_probe).
-behaviour(gen_server).
-behaviour(supercast_channel).
-include("include/monitor.hrl").
-include("../nchecks/include/nchecks.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("common_hrl/include/logs.hrl").

-define(CRASH, "
OUCH a system error has occured!
Check the server logs to see more.
Returning to normal operations.
").

% gen_server
-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

% supercast
-export([get_perms/1,sync_request/2]).

% spawned
-export([exec_nchecks/3]).

% monitor
-export([force/1, trigger_return/2, pause/1, resume/1]).


-record(state, {name,ref}).
-record(nchecks_state, {
    class,
    args,
    dump_dir,
    rrd_config,
    opaque = <<>>,
    last_return = ""
}).

-record(rrd_table, {
    elements,
    suffix,
    prefix,
    base
}).


%
% monitor
%
force(Pid)                 -> gen_server:cast(Pid, force).
trigger_return(Pid,CState) -> gen_server:cast(Pid, {trigger_return, CState}).
pause(_Pid)                -> ok.
resume(_Pid)               -> ok.


start_link(#probe{name=Name} = Probe) ->
    gen_server:start_link({via, supercast_registrar,
                                    {?MODULE, Name}}, ?MODULE, Probe, []).

do_init(Probe) ->
    Ref = make_ref(),
    random:seed(erlang:now()),
    {ok, DumpDir}              = application:get_env(supercast, http_sync_dir),
    {{Class, Args}, RrdConfig} = init_nchecks(Probe),
    TRef                       = monitor:send_after_rand(
                                              Probe#probe.step, {take_of, Ref}),

    NS = #nchecks_state{
        class      = Class,
        args       = Args,
        dump_dir   = DumpDir,
        rrd_config = RrdConfig
    },

    ES = #ets_state{
        name                = Probe#probe.name,
        permissions         = Probe#probe.permissions,
        belong_to           = Probe#probe.belong_to,
        tref                = TRef,
        current_status_from = erlang:now(),
        current_status      = Probe#probe.status,
        local_state         = NS
    },

    monitor_data_master:set_probe_state(ES),

    % partial return for clients allready connected
    PartialReturn = #probe_return{
        status       = ES#ets_state.current_status,
        reply_string = ES#ets_state.last_return},
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
%% supercast channel behaviour
%%----------------------------------------------------------------------------
get_perms(PidName) ->
    #ets_state{permissions=Perm} = monitor_data_master:get_probe_state(PidName),
    Perm.

sync_request(PidName, CState) ->
    gen_server:cast({via, supercast_registrar, PidName}, {sync_request, CState}).

do_sync_request(CState, S) ->
    ES       = monitor_data_master:get_probe_state(S#state.name),

    % generate tmp dir in dump dir
    % TODO cleanup dirs
    DumpDir  = ES#ets_state.local_state#nchecks_state.dump_dir,
    TmpDir   = monitor:generate_temp_dir(),
    DumpPath = filename:join(DumpDir, TmpDir),

    % generate the rrd file
    {Type, RrdCfg}  = ES#ets_state.local_state#nchecks_state.rrd_config,
    case Type of
        simple ->
            file:make_dir(DumpPath),
            RrdFile     = RrdCfg,
            RrdFileBase = filename:basename(RrdFile),

            % copy rrdfile to tmpdir
            DumpFile = filename:join(DumpPath,RrdFileBase),
            file:copy(RrdFile, DumpFile),

            % build the PDU
            Pdu = monitor_pdu:nchecksSimpleDumpMessage(
                                            S#state.name, TmpDir, RrdFileBase),
            supercast_channel:subscribe(ES#ets_state.name, CState),
            supercast_channel:unicast(CState, [Pdu]);

        table ->
            file:make_dir(DumpPath),
            #rrd_table{
               elements = Elements,
               suffix   = Suffix,
               base     = Base,
               prefix   = Prefix} = RrdCfg,

            ElementToFile = lists:map(fun(X) ->
                FileName   = lists:flatten([Prefix,X,Suffix]),
                RrdDstPath = filename:join(DumpPath,FileName),
                RrdSrcPath = lists:flatten([Base,X,Suffix]),
                file:copy(RrdSrcPath,RrdDstPath),
                {X,FileName}
            end, Elements),

            Pdu = monitor_pdu:nchecksTableDumpMessage(
                                        S#state.name, TmpDir, ElementToFile),
            supercast_channel:subscribe(ES#ets_state.name, CState),
            supercast_channel:unicast(CState, [Pdu]);

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

    PartialPR = #probe_return{
        status       = ES#ets_state.current_status,
        reply_string = ES#ets_state.last_return
    },

    MilliRem = monitor:read_timer(ES#ets_state.tref),
    Pdu      = monitor_pdu:probeReturn(
            PartialPR, ES#ets_state.belong_to, ES#ets_state.name, MilliRem),

    supercast_channel:unicast(CState, [Pdu]).

% Force a probe trigger
do_force(#state{ref=Ref} = S) ->
    ES  = monitor_data_master:get_probe_state(S#state.name),

    % cancel the running timer if it exist
    case erlang:cancel_timer(ES#ets_state.tref) of
        false ->
            ok;
        _ ->
            TRef = monitor:send_after(0, {take_of, Ref}),
            monitor_data_master:set_probe_state(ES#ets_state{tref=TRef}),

            % build and send PDU for client with new TRef info (TRef = now)
            PartialReturn = #probe_return{
                status       = ES#ets_state.current_status,
                reply_string = ES#ets_state.last_return
            },
            Pdu = monitor_pdu:probeReturn(
                PartialReturn,
                ES#ets_state.belong_to,
                ES#ets_state.name,
                500
            ),
            supercast_channel:emit(
                            ?MASTER_CHANNEL, {ES#ets_state.permissions, Pdu})
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
    [Probe]   = monitor_data_master:get(probe, S#state.name),
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
    Ts  = PR#probe_return.timestamp,
    Pfs = PR#probe_return.perfs,

    case ES#ets_state.local_state#nchecks_state.rrd_config of
        {simple, RrdFile} ->

            case Pfs of
                [] ->
                    UpdatePdu = monitor_pdu:nchecksSimpleUpdateMessage(
                        S#state.name,PR#probe_return.timestamp, []);

                [{"simple",Perfs}] ->
                    (catch  errd4j:update(RrdFile, Perfs, Ts)),
                    UpdatePdu = monitor_pdu:nchecksSimpleUpdateMessage(
                        S#state.name,PR#probe_return.timestamp,Perfs)
            end;

        {table, Record} ->
            #rrd_table{base=BasePrefix,suffix=Suffix} = Record,
            RrdMultiUpdates = [
                {lists:flatten([BasePrefix, XE, Suffix]), XP, Ts}
                    || {XE,XP} <- Pfs],
            (catch errd4j:multi_update(RrdMultiUpdates)),
            UpdatePdu = monitor_pdu:nchecksTableUpdateMessage(
                                     S#state.name,PR#probe_return.timestamp,Pfs)
    end,


    % send update pdu for subscribers
    supercast_channel:emit(S#state.name,
                                    {ES#ets_state.permissions, UpdatePdu}),


    % schedule next trigger
    TRef        = monitor:send_after(Probe#probe.step, {take_of,Ref}),
    MilliRem    = monitor:read_timer(TRef),


    % send message to subscribers of master channel
    Pdu = monitor_pdu:probeReturn(
        PR,
        ES#ets_state.belong_to,
        ES#ets_state.name,
        MilliRem),
    supercast_channel:emit(?MASTER_CHANNEL, {ES#ets_state.permissions, Pdu}),


    % write state
    monitor_data_master:set_probe_state(
        ES#ets_state{
            tref                =TRef,
            current_status_from = erlang:now(),
            current_status      = PR#probe_return.status,
            last_return         = PR#probe_return.reply_string
        }
    ).

do_take_of(#state{ref=Ref,name=PName}) ->
    ES      = monitor_data_master:get_probe_state(PName),
    Class   = ES#ets_state.local_state#nchecks_state.class,
    Args    = ES#ets_state.local_state#nchecks_state.args,
    Opaque  = ES#ets_state.local_state#nchecks_state.opaque,
    ToPid   = self(),
    erlang:spawn(fun() ->
        {ok, Return} = ?MODULE:exec_nchecks(Class, Args, Opaque),
        erlang:send(ToPid, {probe_return, Ref, Return})
    end).

exec_nchecks(Class, Args, Opaque) ->
    case (catch(nchecks:check(Class,Args,Opaque))) of
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

         Error ->
            % An error occured.
            % TODO should I modify the probe state or set it to 'ERROR'?
            % It is most a sysmo state than a probe state.
            error_logger:error_msg("~p ~p ERROR: ~p", [?MODULE, ?LINE, Error]),
            ProbeReturn = #probe_return{
                status          = "ERROR",
                reply_string    = ?CRASH,
                opaque          = Opaque
            }

    end,
    {ok, ProbeReturn}.


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

handle_cast(Cast, S) ->
    ?LOG_WARNING("Handle unknown cast", Cast),
    {noreply, S}.


handle_call(shut_it_down, _F, #state{name=Name} = S) ->
    supercast_channel:delete(Name),
    {stop, shutdown, ok, S};

handle_call(Call, _From, S) ->
    ?LOG_WARNING("Handle unknown call", Call),
    {noreply, S}.

handle_info({probe_return, Ref, PR}, #state{ref=Ref} = S) ->
    ?LOG_INFO("Probe return", PR),
    do_handle_probe_return(PR, S),
    {noreply, S};

handle_info({take_of, Ref}, #state{ref=Ref} = S) ->
    do_take_of(S),
    {noreply, S};

handle_info(I, SData) ->
    ?LOG_WARNING("Handle unknown info", I),
    {noreply, SData}.


terminate(_Reason, _S) ->
    normal.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.


%%----------------------------------------------------------------------------
%% Nchecks functions
%%----------------------------------------------------------------------------
init_nchecks(#probe{belong_to=TargetName,module_config=NCheck} = Probe) ->


    % Get the target directory TargetDir
    [Target]  = monitor_data_master:get(target, TargetName),
    TargetDir = proplists:get_value(var_directory,
                                            Target#target.sys_properties),

    TargetProp      = Target#target.properties,
    TargetSysProp   = Target#target.sys_properties,
    #nchecks_probe_conf{class=Class, identifier=Identifier, args=Args} = NCheck,


    % if "host" is not defined in probe conf, use the target "host" property
    case proplists:lookup("host", Args) of
        none ->
            TargHost = proplists:lookup("host", TargetProp),
            Args2  = [TargHost|Args];
        _ ->
            Args2 = Args
    end,


    % Generate XML Class definition file path
    XmlDefinitionFile = string:concat(Identifier, ".xml"),
    XmlDefinitionPath = filename:join(
                                    ["etc", "nchecks", XmlDefinitionFile]),


    % Load XML file content
    {#xmlDocument{content=XDocument_Content}, _} =
        xmerl_scan:file(XmlDefinitionPath, [{document, true}]),


    % Extract <NChecks> content
    #xmlElement{content=XNChecks_Content} =
        lists:keyfind('NChecks', 2, XDocument_Content),


    % Extract <Check> content
    % TODO remove #ncheck_probe_conf.class and use XCheck_Attrib(Class)
    #xmlElement{content=XCheck_Content,attributes=_XCheck_Attrib} =
        lists:keyfind('Check', 2, XNChecks_Content),


    Args3 = build_snmp_args(Args2, TargetSysProp, TargetName),


    % append check_id to args for jruby checks, do not hurt for others.
    Args4 = [{"check_id", Identifier} | Args3],

    #probe{name=ProbeName,step=Step} = Probe,
    {ok, RrdState} = rrd4j_init(ProbeName, Step, Args4, TargetDir, XCheck_Content),

    % return
    {{Class, Args4}, RrdState}.


build_snmp_args(Args,TargetSysProp,TargetName) ->
    Adds = ["snmp_port","snmp_version","snmp_seclevel","snmp_community",
            "snmp_usm_user","snmp_authkey","snmp_authproto","snmp_privkey",
            "snmp_privproto", "snmp_timeout", "snmp_retries"],
    Adds2 = build_snmp_args2(TargetSysProp,Adds,[{"target_id",TargetName}]),
    SortedAdds = lists:sort(Adds2),
    SortedArgs = lists:sort(Args),
    lists:merge(SortedAdds, SortedArgs).
build_snmp_args2(_,[], Value) -> Value;
build_snmp_args2(TargetSysProp,[Prop|Others], Value) ->
    case lists:keyfind(Prop,1,TargetSysProp) of
        false ->
            build_snmp_args2(TargetSysProp,Others,Value);
        Val ->
            build_snmp_args2(TargetSysProp,Others,[Val|Value])
    end.



%%----------------------------------------------------------------------------
%% errd4j functions
%%----------------------------------------------------------------------------


rrd4j_init(ProbeName, Step, Args, TargetDir, XCheck_Content) ->

    % Generate check path dir and maybe create
    ProbeDir = filename:join([TargetDir, ProbeName]),
    case filelib:is_dir(ProbeDir) of
        false -> file:make_dir(ProbeDir);
        true  -> ok
    end,



    % Extract <Performances> content and attributes. This is where we
    % have all our relevant informations.
    #xmlElement{
        content    = XPerformances_Content,
        attributes = XPerformances_Attrib
    } = lists:keyfind('Performances', 2, XCheck_Content),


    % Extract <Performances Type="?"> attributes and switch
    #xmlAttribute{
         value = XPerformances_Attr_Type
    } = lists:keyfind('Type', 2, XPerformances_Attrib),


    case XPerformances_Attr_Type of

        "table" ->
            % "table" Performance type mean one rrd file definition
            % for x files. For monitoring lists of things that return
            % values. For example, interfaces staticstics.


            % Get the prefix used to build file names
            XFilePrefix_Content = x_get_content_text(
                                        XPerformances_Content, 'FilePrefix'),


            % Get the suffix used to build file names
            XFileSuffix_Content = x_get_content_text(
                                        XPerformances_Content, 'FileSuffix'),


            % Get the flag from where we will create rrds
            XFlagSource_Content = x_get_attr_val(
                                        XPerformances_Content, 'FlagSource', 'Name'),


            % Get the flag conf himself. It should exist because it must be
            % a mandatory flag.
            {_Key,FlagValue} = lists:keyfind(XFlagSource_Content, 1, Args),


            % Get the flag from where we will create rrds
            XFlagSeparator_Content = x_get_content_text(
                                        XPerformances_Content,'FlagSeparator'),


            % With FlagSeparator and Args[Flag] content, generate a list
            % of elements
            RRDList = string:tokens(FlagValue, XFlagSeparator_Content),


            % Generate the path prefix/suffix
            Suffix = XFileSuffix_Content,
            Prefix = XFilePrefix_Content,
            BasePrefix = filename:join([ProbeDir, Prefix]),


            % Create ds definitions
            DSDefinitions = build_DS_Def(XPerformances_Content, Step),

            % Create rrd files if needed
            lists:foreach(fun(Element) ->
                FilePath = lists:flatten([BasePrefix, Element, Suffix]),
                case filelib:is_regular(FilePath) of
                    true  -> ok;
                    false ->
                        case (catch errd4j:create(
                                  FilePath, Step, DSDefinitions, "default")) of
                            ok    -> ok;
                            Error ->
                                error_logger:error_msg(
                                  "~p ~p ERROR: ~p", [?MODULE, ?LINE, Error])
                        end
                end
            end, RRDList),


            {ok, {table, #rrd_table{elements=RRDList,base=BasePrefix,
                                            suffix=Suffix,prefix=Prefix}}};

        "simple" ->
            % "simple" Performance type mean only one rrd file (but off course
            % possibly multiple datasources)

            % Get the FileName
            XFileName_Content = x_get_content_text(
                                            XPerformances_Content, 'FileName'),

            % Generate rrd file path
            RrdFilePath = filename:join([ProbeDir, XFileName_Content]),

            case filelib:is_regular(RrdFilePath) of
                true ->
                    % Allready done
                    {ok, {simple, RrdFilePath}};
                false ->
                    % Create ds definitions
                    DSDefinitions = build_DS_Def(XPerformances_Content, Step),
                    % Create rrd file.

                    case (catch errd4j:create(
                                  RrdFilePath,Step, DSDefinitions,"default")) of
                        ok    -> ok;
                        Error ->
                            error_logger:error_msg(
                              "~p ~p ERROR: ~p", [?MODULE, ?LINE, Error])
                    end,


                    % Return rrd file path
                    {ok, {simple, RrdFilePath}}
            end;
        Unknown ->
            error_logger:error_msg("~p ~p ERROR: unknown rrd type: ~p",
                                                     [?MODULE, ?LINE, Unknown]),
            error
    end.


x_get_content_text(Content, Key) ->
    #xmlElement{content=C1} = lists:keyfind(Key, 2, Content),
    #xmlText{value=V}       = lists:keyfind(xmlText, 1, C1),
    V.


x_get_attr_val(Content, Key, Attr) ->
    #xmlElement{attributes=A1} = lists:keyfind(Key,  2, Content),
    #xmlAttribute{value=V}     = lists:keyfind(Attr, 2, A1),
    V.


build_DS_Def(XPerformances_Content, Step) ->
    % Get DataSourceTable
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
    DSDefinitions.

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
-module(probe_nchecks).
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
-export([exec_nchecks/2]).

-record(state, {name,ref}).
-record(nchecks_state, {
    class,
    args,
    dump_dir,
    rrd_file_path
}).





start_link(#probe{name=Name} = Probe) ->
    gen_server:start_link({via, supercast_registrar, {?MODULE, Name}}, ?MODULE, Probe, []).

do_init(Probe, Ref) ->
    random:seed(erlang:now()),
    {ok, DumpDir}   = application:get_env(supercast, http_sync_dir),
    {Class, Args}   = nchecks_init(Probe),
    RrdFile         = rrd4j_init(Probe),
    TRef            = monitor:send_after_rand(Probe#probe.step, {take_of, Ref}),
    NS = #nchecks_state{
        class = Class,
        args = Args,
        dump_dir = DumpDir,
        rrd_file_path = RrdFile
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
    ok.





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
    RrdFile  = ES#ets_state.local_state#nchecks_state.rrd_file_path,
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
    Pdu = monitor_pdu:nchecksDumpMessage(S#state.name, TmpDir, RrdFileBase),
    supercast_channel:subscribe(ES#ets_state.name, CState),
    supercast_channel:unicast(CState, [Pdu]).
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
    RrdFile = ES#ets_state.local_state#nchecks_state.rrd_file_path,
    Perfs   = PR#probe_return.perfs,
    ok = errd4j:update(RrdFile, Perfs, PR#probe_return.timestamp),

    % SEND MESSAGE to subscribers of self() to update their rrds
    Pdu2 = monitor_pdu:nchecksUpdateMessage(S#state.name,PR#probe_return.timestamp,Perfs),
    supercast_channel:emit(S#state.name, {ES#ets_state.permissions, Pdu2}),

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
    ToPid   = self(),
    erlang:spawn(fun() ->
        {ok, Return}  = ?MODULE:exec_nchecks(Class, Args),
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
    Ref = make_ref(),
    do_init(Probe, Ref),
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
nchecks_init(Probe) ->
    [Target]    = monitor_data_master:get(target, Probe#probe.belong_to),
    TargetProp  = Target#target.properties,
    Conf        = Probe#probe.module_config,
    #nchecks_probe_conf{class = Class, args = Args} = Conf,

    % if "host" is not defined in probe conf, use the target "host" property
    case proplists:lookup("host", Args) of
        none ->
            TargHost = proplists:lookup("host", TargetProp),
            NewArgs  = [TargHost|Args];
        _ ->
            NewArgs = Args
    end,
    {Class, NewArgs}.


exec_nchecks(Class, Args) ->
    case nchecks:check(Class,Args) of
        {error, Error} ->
            ProbeReturn = #probe_return{
                status          = "ERROR",
                reply_string    = Error
            };
        {ok, Reply} ->
            #nchecks_reply{
               status=Status,
                performances=Perfs,
                reply_string=Str,
                timestamp=Ts
            } = Reply,
            ProbeReturn = #probe_return{
                status          = Status,
                reply_string    = Str,
                timestamp       = Ts,
                perfs           = Perfs
            }
    end,
    {ok, ProbeReturn}.

%%----------------------------------------------------------------------------
%% errd4j functions
%%----------------------------------------------------------------------------
rrd4j_init(#probe{name=Name,step=Step,belong_to=TargetName,module_config=NCheck}) ->

    % get the target directory TargetDir
    [Target]    = monitor_data_master:get(target, TargetName),
    TargetDir   = proplists:get_value(var_directory, Target#target.sys_properties),

    % generate rrd file path
    ProbeFile       = string:concat(Name, ".rrd"),
    ProbeFilePath   = filename:join([TargetDir, ProbeFile]),

    % does the rrd file allready exists?
    case filelib:is_regular(ProbeFilePath) of
        true ->
            % only return the processed filepath
            ProbeFilePath;
        false ->
            % we will create the rrd defined in the nchecks class xml file
            Class           = NCheck#nchecks_probe_conf.class,
            ClassFile       = string:concat(Class, ".xml"),
            ClassFilePath   = filename:join(["cfg", "nchecks", ClassFile]),

            % read it
            {#xmlDocument{content=DocumentContent}, _} = xmerl_scan:file(ClassFilePath, [{document, true}]),
            % extract <check_def> content
            #xmlElement{content=CheckDefContent} = lists:keyfind(check_def, 2, DocumentContent),
            % extract <performances> content
            #xmlElement{content=PerfContent} = lists:keyfind(performances, 2, CheckDefContent),
            % extract <data_source>
            #xmlElement{content=DSContent} = lists:keyfind(data_sources, 2, PerfContent),
            % only keep ds xmlElements
            DSElements = lists:filter(fun(E) -> is_record(E, xmlElement) end, DSContent),
            % extract ds variables (ds name, ds type ABSOLUTE|COUNTER|DERIVE|GAUGE)
            % ex: DSDef = [{"MaxRoundTrip", "GAUGE"}]
            DSDef = lists:map(fun(#xmlElement{content=DS}) ->
                #xmlElement{content=[DSNameTextElement]} = lists:keyfind(dsName, 2, DS),
                #xmlElement{content=[DSTypeTextElement]} = lists:keyfind(dsType, 2, DS),
                DSName = DSNameTextElement#xmlText.value,
                DSType = DSTypeTextElement#xmlText.value,
                {DSName, DSType, Step * 2, 'Nan', 'Nan'}
            end, DSElements),

            % create rrd file
            ok = errd4j:create(ProbeFilePath, Step, DSDef, "default"),

            % return the processed filepath
            ProbeFilePath
    end.

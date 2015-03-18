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
-include("../nchecks/include/nchecks.hrl").
-behaviour(gen_server).
-behaviour(supercast_channel).
-include("include/monitor.hrl").
-include_lib("xmerl/include/xmerl.hrl").


-export([start_link/1]).
% supercast_channel
-export([get_perms/1,sync_request/2]).
% gen_server
-export([init/1,handle_call/3, handle_cast/2,handle_info/2,terminate/2,code_change/3]).
% API
-export([triggered_return/2,shutdown/1,force/1,exec_nchecks/1]).

% records
-record(state, {name}).
-record(ets_state, {
    name,
    class,
    args,
    permissions,
    target_name,
    inspectors_state,
    loggers_state,
    tref,
    status_from,
    status}).




%%----------------------------------------------------------------------------
%% GEN_SERVER INIT
%%----------------------------------------------------------------------------
start_link(#probe{name=Name} = Probe) ->
    gen_server:start_link({via, supercast_registrar, {?MODULE, Name}}, ?MODULE, Probe, []).

% to let multiple probes initialize in the same time, init is delayed.
init(Probe) ->
    gen_server:cast(self(), continue_init),
    {ok, Probe}.

init2(Probe) ->
    random:seed(erlang:now()),
    {Class, Args} = init_probe(Probe),
    RrdFile = rrd_init(Probe),
    {ok, InspectInitState}  = monitor_inspector:init_all(Probe),
    %{ok, LoggersInitState}  = monitor_logger:init_all(Probe),
    TRef = initiate_start_sequence(Probe#probe.step, random),
    ES = #ets_state{
        class            = Class,
        args             = Args,
        name             = Probe#probe.name,
        permissions      = Probe#probe.permissions,
        target_name      = Probe#probe.belong_to,
        inspectors_state = InspectInitState,
        loggers_state    = RrdFile,
        tref             = TRef,
        status_from      = erlang:now(),
        status           = Probe#probe.status
    },
    monitor_data_master:set_probe_state(ES),
    % BEGIN partial return for clients
    PartialReturn = partial_pr(ES),
    MilliRem = read_timer(ES#ets_state.tref),
    Pdu = monitor_pdu:probeReturn(
        PartialReturn,
        ES#ets_state.target_name,
        ES#ets_state.name,
        MilliRem
    ),
    % END partial return for clients
    supercast_channel:emit(?MASTER_CHANNEL, {ES#ets_state.permissions, Pdu}),
    ok.


%%----------------------------------------------------------------------------
%% supercast channel behaviour API
%%----------------------------------------------------------------------------
% @private
% @doc
% Supercast behaviour required.
% @end
get_perms(PidName) ->
    #ets_state{permissions=Perm} = monitor_data_master:get_probe_state(PidName),
    Perm.

% @private
% @doc
% Supercast behaviour required.
% @end
sync_request(PidName, CState) ->
    gen_server:cast({via, supercast_registrar, PidName}, {sync_request, CState}).
sync_request2(CState, S) ->
    ES = monitor_data_master:get_probe_state(S#state.name),
    LS = ES#ets_state.loggers_state,
    {ok, Pdus, LS2} = monitor_logger:dump_all(LS, CState),
    ok  = supercast_channel:subscribe(ES#ets_state.name, CState),
    ok  = supercast_channel:unicast(CState, Pdus),
    monitor_data_master:set_probe_state(ES#ets_state{loggers_state=LS2}),
    ok.





-spec triggered_return(PidName::string(), CState::#client_state{}) -> ok.
% @private
% @doc
% Used by the monitor main channel to initialize clients. This function send
% a Partial Probe return PDU to the specified client, including the next expected
% return time.
% @end
triggered_return(PidName, CState) ->
    gen_server:cast({via, supercast_registrar, PidName}, {triggered_return, CState}).
triggered_return2(CState, S) ->
    ES = monitor_data_master:get_probe_state(S#state.name),

    PartialPR = #probe_return{ 
        status          = ES#ets_state.status,
        reply_string    = "",
        timestamp       = 0,
        key_vals        = []
    },

    MilliRem = read_timer(ES#ets_state.tref),
    Pdu = monitor_pdu:probeReturn(
        PartialPR,
        ES#ets_state.target_name,
        ES#ets_state.name,
        MilliRem
    ),
    supercast_channel:unicast(CState, [Pdu]),
    ok.


%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------
-spec shutdown(PidName::string()) -> ok.
% @private
% @doc
% Used by monitor datamaster. It shut down the probe specified wile data master
% delete it from the db.
% @end
shutdown(PidName) ->
    case supercast_registrar:whereis_name(PidName) of
        undefined ->
            ok;
        Pid ->
            gen_server:call(Pid, shut_it_down)
    end.

-spec force(PidName::string()) -> ok.
% @private
% @doc
% Force a probe check as soon as possible. Used mainly from monitor API.
% @end
force(PidName) ->
    case supercast_registrar:whereis_name(PidName) of
        undefined ->
            ok;
        Pid ->
            gen_server:cast(Pid, force)
    end.
force2(S) ->
    ES  = monitor_data_master:get_probe_state(S#state.name),
    case erlang:cancel_timer(ES#ets_state.tref) of
        false ->
            ok;
        _ ->
            TRef = initiate_start_sequence(undefined, now),
            monitor_data_master:set_probe_state(ES#ets_state{tref=TRef}),
            PartialReturn = partial_pr(ES),
            Pdu = monitor_pdu:probeReturn(
                PartialReturn,
                ES#ets_state.target_name,
                ES#ets_state.name,
                500
            ),
            supercast_channel:emit(?MASTER_CHANNEL, {ES#ets_state.permissions, Pdu})
    end.



%%----------------------------------------------------------------------------
%% INTERNALS
%%----------------------------------------------------------------------------
-spec handle_probe_return(PR::any(), S::any()) -> ok.
% @private
% @doc
% Called by handle_info(probe_return). This function do not need to be exported.
% @end
handle_probe_return(PR, S) ->
    ES  = monitor_data_master:get_probe_state(S#state.name),

    % INSPECT TODO do use case better than behaviour
    [Probe]  = monitor_data_master:get(probe, S#state.name),
    IState = ES#ets_state.inspectors_state,
    {ok, IState2, Probe2} = monitor_inspector:inspect_all(IState, Probe, PR),

    % LOGGER TODO do use case better than behaviour
    %LState = ES#ets_state.loggers_state,
    %{ok, Pdus, LState2} = monitor_logger:log_all(LState,PR),
    %emit_all(ES#ets_state.name, ES#ets_state.permissions, Pdus),
    rrd_log(ES#ets_state.loggers_state, PR),

    % LAUNCH
    TRef        = initiate_start_sequence(Probe#probe.step, normal),
    MilliRem    = read_timer(TRef),

    % SEND MESSAGES
    Pdu = monitor_pdu:probeReturn(
        PR,
        ES#ets_state.target_name,
        ES#ets_state.name,
        MilliRem
    ),
    supercast_channel:emit(?MASTER_CHANNEL, {ES#ets_state.permissions, Pdu}),

    % WRITE
    monitor_data_master:set_probe_state(
        ES#ets_state{
            inspectors_state=IState2,
            tref=TRef,
            status_from = erlang:now(),
            status=Probe2#probe.status
        }
    ),

    case Probe of
        Probe2 -> ok;
        _ ->
            monitor_data_master:update(probe, Probe2)
    end.






%%----------------------------------------------------------------------------
%% HANDLE_CAST
%%----------------------------------------------------------------------------
handle_cast(continue_init, Probe) ->
    init2(Probe),
    {noreply, #state{name=Probe#probe.name}};

handle_cast({sync_request, CState}, S) ->
    sync_request2(CState, S),
    {noreply, S};

handle_cast({triggered_return, CState}, S) ->
    triggered_return2(CState, S),
    {noreply, S};

handle_cast(force, S) ->
    force2(S),
    {noreply, S};

handle_cast(_Cast, S) ->
    {noreply, S}.

%%----------------------------------------------------------------------------
%% HANDLE_CALL
%%----------------------------------------------------------------------------
handle_call(shut_it_down, _F, #state{name=Name} = S) ->
    supercast_channel:delete(Name),
    {stop, shutdown, ok, S};


handle_call(_Call, _From, S) ->
    {noreply, S}.

%%----------------------------------------------------------------------------
%% HANDLE_INFO
%%----------------------------------------------------------------------------
handle_info({probe_return, PR}, S) ->
    handle_probe_return(PR, S),
    {noreply, S};

handle_info(take_of, S) ->
    handle_take_of(S),
    {noreply, S};

handle_info(_I, SData) ->
    {noreply, SData}.

%%----------------------------------------------------------------------------
%% OTHER GEN_SERVER
%%----------------------------------------------------------------------------
terminate(_Reason, _S) ->
    normal.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PROBE LAUNCH %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec initiate_start_sequence(Step::integer(),Mode::normal|random|now) -> any().
initiate_start_sequence(Step, random) ->
    Step2   = random:uniform(Step),
    initiate_start_sequence(Step2);
initiate_start_sequence(Step, normal) ->
    initiate_start_sequence(Step);
initiate_start_sequence(_, now) ->
    initiate_start_sequence(0).

initiate_start_sequence(Step) ->
    erlang:send_after(Step * 1000, self(), take_of).

handle_take_of(S) ->
    ES      = monitor_data_master:get_probe_state(S#state.name),
    Class   = ES#ets_state.class,
    Args    = ES#ets_state.args,
    Parent  = self(),
    erlang:spawn(fun() ->
        {ok, Return}  = ?MODULE:exec_nchecks({Class, Args}),
        erlang:send(Parent, {probe_return, Return})
    end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTILS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%emit_all(_, _, []) -> ok;
%emit_all(Name, Perm, [Pdu|T]) ->
%    supercast_channel:emit(Name,{Perm, Pdu}),
%    emit_all(Name,Perm,T).

read_timer(TRef) ->
    case erlang:read_timer(TRef) of
        false -> 0;
        Any   -> Any
    end.


partial_pr(ES) ->
    #probe_return{ 
        status          = ES#ets_state.status,
        reply_string    = "",
        timestamp       = 0,
        key_vals        = []
    }.


%%----------------------------------------------------------------------------
%% Nchecks functions
%%----------------------------------------------------------------------------
init_probe(Probe) ->
    [Target]    = monitor_data_master:get(target, Probe#probe.belong_to),
    TargetProp  = Target#target.properties,
    Conf        = Probe#probe.monitor_probe_conf,
    #nchecks_probe_conf{class = Class, args = Args} = Conf,

    % if "host" is not defined in probe conf, use the target "ip" and
    % "ipVersion" property.
    case proplists:lookup("host", Args) of
        none ->
            TargHost = proplists:lookup("host", TargetProp),
            NewArgs  = [TargHost|Args];
        _ ->
            NewArgs = Args
    end,
    {Class, NewArgs}.


exec_nchecks({Class, Args}) ->
    case nchecks:check(Class,Args) of
        {error, Error} ->
            ProbeReturn = #probe_return{
                status      = "ERROR",
                reply_string   = Error
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
                key_vals        = Perfs
            }
    end,
    {ok, ProbeReturn}.

%%----------------------------------------------------------------------------
%% errd4j functions
%%----------------------------------------------------------------------------
rrd_init(#probe{name=Name, step=Step, belong_to=TargetName, monitor_probe_conf=NCheck} = _P) ->

    % get the target directory TargetDir
    [Target]    = monitor_data_master:get(target, TargetName),
    TargetDir   = proplists:get_value(var_directory, Target#target.sys_properties),

    % does the rrd file allready exists?
    ProbeFile       = string:concat(Name, ".rrd"),
    ProbeFilePath   = filename:join([TargetDir, ProbeFile]),
    case filelib:is_regular(ProbeFilePath) of
        true ->
            % only return the processed filepath
            ProbeFilePath;
        false ->
            % we will create the rrd file defined in the nchecks class xml file
            % first get the dump dir for future client sync request
            {ok, _DumpDir} = application:get_env(supercast, http_sync_dir),
            % get the NChecks working class Class
            #nchecks_probe_conf{class = Class} = NCheck,
            % get the rrd definition from the NChecks XML file of the Class
            ClassFile       = string:concat(Class, ".xml"),
            ClassFilePath   = filename:join(["cfg", "nchecks", ClassFile]),

            % read the xml file
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
            % ex: DSDef = [{"MaxRoundTrip", "ABSOLUTE}]
            DSDef = lists:map(fun(#xmlElement{content=DS}) ->
                #xmlElement{content=DSNameContent} = lists:keyfind(dsName, 2, DS),
                #xmlElement{content=DSTypeContent} = lists:keyfind(dsType, 2, DS),
                [DSNameTextElement] = DSNameContent,
                [DSTypeTextElement] = DSTypeContent,
                DSName = DSNameTextElement#xmlText.value,
                DSType = DSTypeTextElement#xmlText.value,
                {DSName, DSType, Step * 2, 'Nan', 'Nan'}
            end, DSElements),

            ?LOG({"init_loggers...... for", ProbeFilePath, DSDef, Step}),

            errd4j:create(ProbeFilePath, Step, DSDef),

            % return the processed filepath
            ProbeFilePath
    end.

rrd_log(File, Updates) ->
    ?LOG({update, File, Updates}).

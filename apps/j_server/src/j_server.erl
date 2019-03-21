%%==============================================================================
%% Sysmo NMS Network Management and Monitoring solution (https://sysmo-nms.github.io)
%%
%% Copyright (c) 2012-2017 Sebastien Serre <ssbx@sysmo.io>
%%
%% Sysmo NMS is free software: you can redistribute it and/or modify it under
%% the terms of the GNU General Public License as published by the Free Software
%% Foundation, either version 3 of the License, or (at your option) any later
%% version.
%%
%% Sysmo NMS is distributed in the hope that it will be useful, but WITHOUT ANY
%% WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
%% A PARTICULAR PURPOSE. See the GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License along with
%% Sysmo.  If not, see <http://www.gnu.org/licenses/>.
%%==============================================================================
-module(j_server).
-include_lib("common_hrl/include/logs.hrl").

-behaviour(gen_server).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

% utils
-export([start_link/0]).

% other
-export([get_pid/1]).

-record(state, {main_pid, rrd4j_pid, snmp4j_pid, nchecks_pid,
    eventdb_pid, mail_pid, assert, ready=false}).

start_link() ->
    Result = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    % Wait for java side mailbox to be ready and send the "java_connected"
    % info message.
    ok = assert_init(),
    Result.

get_pid(For) ->
    gen_server:call(?MODULE, {get_pid, For}).

assert_init() ->
    gen_server:call(?MODULE, assert_init, infinity).


% gen_server
init([]) ->
    % build relative java server script path
    process_flag(trap_exit, true),
    ?LOG_INFO("Starting j_server"),
    Prefix = case os:type() of
                 {win32,_} -> ".bat";
                 _         -> ""
             end,
    Relative = string:concat("sysmo-jserver/bin/sysmo-jserver", Prefix),

    % get the location of the java app and generate cmd
    {ok,JDir} = application:get_env(sysmo, java_dir),
    Cmd = filename:join(filename:absname(JDir), Relative),

    % build arguments
    Node     = erlang:atom_to_list(erlang:node()),

    % the java app will be started from WorkDir (redundant?)
    WorkDir  = filename:absname(""),

    erlang:open_port({spawn_executable, Cmd},
        [{cd, WorkDir}, {args,[Node]}, exit_status, stderr_to_stdout]),

    {ok, #state{}}.

handle_call(assert_init, _From, #state{ready=true} = S) ->
    {reply, ok, S};
handle_call(assert_init, From, S) ->
    {noreply, S#state{assert = From}};

handle_call({get_pid, rrd4j}, _From, #state{rrd4j_pid=Pid} = S) ->
    {reply, Pid, S};
handle_call({get_pid, snmp4j}, _From, #state{snmp4j_pid=Pid} = S) ->
    {reply, Pid, S};
handle_call({get_pid, nchecks}, _From, #state{nchecks_pid=Pid} = S) ->
    {reply, Pid, S};
handle_call({get_pid, eventdb}, _From, #state{eventdb_pid=Pid} = S) ->
    {reply, Pid, S};

handle_call(_Call, _, S) ->
    ?LOG_WARNING("Received unknow call", _Call),
    {noreply, S}.


handle_cast(_Cast, S) ->
    ?LOG_WARNING("Received unknow cast", _Cast),
    {noreply, S}.


handle_info({java_connected, MainMbox, Rrd4jPid, Snmp4jPid, NchecksPid,
    EventdbPid, MailPid}, #state{assert=From}) ->
    case From of undefined -> ok; _ -> gen_server:reply(From, ok) end,
    erlang:link(MainMbox),
    ?LOG_INFO("main mbox pid is: ", MainMbox),
    {noreply, #state{
        ready = true,
        main_pid = MainMbox,
        rrd4j_pid = Rrd4jPid,
        snmp4j_pid = Snmp4jPid,
        nchecks_pid = NchecksPid,
        eventdb_pid = EventdbPid,
        mail_pid = MailPid}};

handle_info({_Port, {exit_status, Status}}, S) ->
    ?LOG_ERROR("java node has crashed", {status, Status, state, S}),
    {stop, "Java node has crashed", S};

handle_info(_Info, S) ->
    ?LOG_WARNING("Received unknow info", _Info),
    {noreply, S}.

terminate(_Reason, #state{main_pid=MainMbox}) ->
    ?LOG_INFO("Received terminate", _Reason),
    exit(MainMbox, "terminate"),
    wait_port_close(),
    ok.

code_change(_, S, _) -> {ok, S}.

wait_port_close() ->
    receive
        {_, {exit_status, _Status}} ->
            ?LOG_INFO("Port closed with exit status: ", _Status),
            ok;
        _ ->
            wait_port_close()
    end.

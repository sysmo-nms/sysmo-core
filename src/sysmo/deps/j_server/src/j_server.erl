-module(j_server).
-include_lib("common_hrl/include/logs.hrl").

-behaviour(gen_server).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

% utils
-export([start_link/0]).

% other
-export([get_pid/1, get_http_port/0]).

-record(state, {rrd4j_pid, snmp4j_pid, nchecks_pid,
                eventdb_pid, mail_pid, jetty_port, assert, ready=false}).

-define(JAVA_START_TIMEOUT, 25000).

get_pid(For) ->
    gen_server:call(?MODULE, {get_pid, For}).

get_http_port() ->
    gen_server:call(?MODULE, get_http_port).

assert_init() ->
    gen_server:call(?MODULE, assert_init, ?JAVA_START_TIMEOUT).

start_link() ->
    Ret = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    % Wait for java side mailbox ready and send the "java_connected"
    % info message.
    ok  = assert_init(),
    Ret.

init([]) ->
    % build relative java server script path
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
    Cookie   = erlang:atom_to_list(erlang:get_cookie()),

    % the java app will be started from WorkDir (redundant?)
    WorkDir  = filename:absname(""),

    erlang:open_port({spawn_executable, Cmd},
                [{cd, WorkDir},
                 {args,[Node, Cookie]},
                 exit_status,
                 stderr_to_stdout]),
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

handle_call(get_http_port, _From, #state{jetty_port=Port} = S) ->
    {reply, Port, S};

handle_call(_Call, _, S) ->
    ?LOG_WARNING("Received unknow call", _Call),
    {noreply, S}.


handle_cast(_Cast, S) ->
    ?LOG_WARNING("Received unknow cast", _Cast),
    {noreply, S}.


handle_info({java_connected, Rrd4jPid, Snmp4jPid, NchecksPid, EventdbPid,
             MailPid, JettyPort}, #state{assert=undefined}) ->
    application:set_env(supercast, http_port, JettyPort),
    {noreply, #state{
                 ready = true,
                 rrd4j_pid = Rrd4jPid,
                 snmp4j_pid = Snmp4jPid,
                 nchecks_pid = NchecksPid,
                 eventdb_pid = EventdbPid,
                 mail_pid = MailPid,
                 jetty_port = JettyPort}};
handle_info({java_connected, Rrd4jPid, Snmp4jPid, NchecksPid, EventdbPid,
             MailPid, JettyPort}, #state{assert=From}) ->
    gen_server:reply(From, ok),
    application:set_env(supercast, http_port, JettyPort),
    {noreply, #state{
                 ready = true,
                 rrd4j_pid = Rrd4jPid,
                 snmp4j_pid = Snmp4jPid,
                 nchecks_pid = NchecksPid,
                 eventdb_pid = EventdbPid,
                 mail_pid = MailPid,
                 jetty_port = JettyPort}};

handle_info({_Port, {exit_status, Status}}, S) ->
    ?LOG_ERROR("java node has crashed", {status, Status, state, S}),
    {noreply, S};
    %{stop, "Java node has crashed", S};

handle_info(_Info, S) ->
    ?LOG_WARNING("Received unknow info", _Info),
    {noreply, S}.


terminate(_,_) ->
    ok.

code_change(_,S,_) ->
     {ok, S}.

-module(errd_server).
-behaviour(gen_server).
-include("../include/errd_internal.hrl").
-include_lib("eunit/include/eunit.hrl").
% gen_server exports
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2,
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

-export([
    start_link/0,
    cd/1,
    raw/1,
    info/1,
    format_raw/2,
    command/1
]).

-record(state, {rrd_port}).
-define(RRD_COMMAND_TIMEOUT, 5 * 1000).

%%====================================================================
%% External functions
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

cd(Directory) ->
    gen_server:call(?MODULE, {cd, Directory}).

info(Filename) ->
    case gen_server:call(?MODULE, {info, Filename}) of
        {ok, Data} ->
            errd_info:parse(Data);
        Other ->
            Other
    end.

raw(Str) ->
    gen_server:call(?MODULE, {raw, Str}).

format_raw(Fmt, Args) ->
    gen_server:call(?MODULE, {raw, Fmt, Args}).

command(Cmd) ->
    raw(errd_command:format(Cmd)).

%%====================================================================
%% Server functions
%%====================================================================
init([]) ->
    {ok, RrdTool}   = application:get_env(errd, command),
    {ok, Directory} = application:get_env(errd, base_dir),
    case open_port({spawn, RrdTool},
                   [use_stdio, exit_status, {line, 16000}]) of
        Port when is_port(Port) ->
            {ok, _} = rrd_command(Port, "cd ~s~n", [Directory]),
            {ok, #state{rrd_port=Port}};
        Else ->
            {stop, {no_rrdtool, Else}}
    end.

%handle_call({create, Spec = #rrd_create{}}, _From, State=#state{rrd_port=Port}) ->
%    Fmt = ok, Args = ok,
%    {reply, rrd_command(Port, Fmt, Args), State};
handle_call({raw, Fmt, Args}, _From, State=#state{rrd_port=Port}) ->
    {reply, rrd_command(Port, Fmt, Args), State};
handle_call({raw, Cmd}, _From, State=#state{rrd_port=Port}) ->
    {reply, rrd_command(Port, Cmd), State};
handle_call({cd, Directory}, _From, State=#state{rrd_port=Port}) ->
    Result = rrd_command(Port, "cd ~s~n", [Directory]),
    {reply, Result, State};
handle_call({info, Filename}, _From, State=#state{rrd_port=Port}) ->
    Result = rrd_command(Port, "info ~s~n", [Filename]),
    {reply, Result, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(Request, _From, State) ->
    ?WARN("Unexpected call: ~p", [Request]),
    {reply, {error, unknown_call}, State}.

handle_cast(Msg, State) ->
    ?WARN("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    ?WARN("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, #state{rrd_port=P}) when is_port(P) ->
    port_close(P),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%%% Internal functions
%%====================================================================

rrd_command(Port, Fmt, Args) ->
    rrd_command(Port, lists:flatten(io_lib:format(Fmt, Args))).

rrd_command(Port, Command) ->
    true = port_command(Port, Command),
    wait_rrd_command(Port, Command, [], []).

wait_rrd_command(Port, Cmd, Lines, SoFar) ->
    receive
        {P, {data, {eol, "OK " ++ PerfData}}} when P == Port, SoFar == [] ->
            parse_rrd_response(Cmd, {ok, PerfData}, lists:reverse(Lines));
        {P, {data, {eol, "ERROR: " ++ Error}}} when P == Port, SoFar == [] ->
            parse_rrd_response(Cmd, {error, Error}, lists:reverse(Lines));
        {P, {data, {eol, Data}}} when P == Port ->
            wait_rrd_command(Port, Cmd, [Data|Lines], SoFar);
        {P, {data, {noeol, Data}}} when P == Port ->
            wait_rrd_command(Port, Cmd, Lines, SoFar ++ Data)
    after ?RRD_COMMAND_TIMEOUT ->
            ?WARN("rrdtool didn't respond to [~s] within ~pms.~nPartial data: ~p~n.",
                  [Cmd, ?RRD_COMMAND_TIMEOUT, SoFar]),
            {error, rrd_timeout}
    end.

parse_rrd_response(_Cmd, {ok, _PerfData}, Lines) ->
    %?INFO("Command [~s] completed: ~s~n~s~n", [Cmd, PerfData, Lines]),
    {ok, Lines};
parse_rrd_response(Cmd, {error, Error}, Lines) ->
    ?WARN("Command [~s] failed: ~s~n~s~n", [Cmd, Error, Lines]),
    {error, Error}.

rrd_cmd_test() ->
    {ok, Pid} = ?MODULE:start_link(),
    ?assertMatch({ok, []}, gen_server:call(Pid, {cd, "/"})),
    ?MODULE:stop(Pid).

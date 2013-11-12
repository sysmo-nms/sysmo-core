%%%----------------------------------------------------------------------
%%% File    : echo.erl
%%% Author  : Pete Kazmier <pete-trapexit@kazmier.com>
%%% Purpose : Port Tutorial
%%% Created : Fri Jan 13 12:39:27 EST 2006
%%%----------------------------------------------------------------------

-module(tlogger_rrd).
-behavior(gen_server).
-export([start_link/0]).

-export([exec/1]).

-export([init/1, 
         handle_call/3,
         handle_cast/2, 
         handle_info/2,
         code_change/3,
         terminate/2]).

%% Server state
-record(state, {port}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

exec(Msg) ->
    gen_server:call(?MODULE, {exec, Msg}).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

init([]) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, "/usr/bin/rrdtool -"}, [use_stdio, stream, {line, 100}]),
    {ok, #state{port = Port}}.

handle_call({exec, Msg}, _From, #state{port = Port} = State) ->
    port_command(Port, Msg),
    case collect_response(Port) of
        {response, Response} -> 
            {reply, Response, State};
        timeout -> 
            {stop, port_timeout, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, {port_terminated, Reason}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate({port_terminated, _Reason}, _State) ->
    ok;
terminate(_Reason, #state{port = Port} = _State) ->
    port_close(Port).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

collect_response(Port) ->
    collect_response(Port, [], []).

collect_response(Port, RespAcc, LineAcc) ->
    receive
        {Port, {data, {eol, "OK"}}} ->
            {response, lists:reverse(RespAcc)};

        {Port, {data, {eol, Result}}} ->
            Line = lists:reverse([Result | LineAcc]),
            collect_response(Port, [Line | RespAcc], []);

        {Port, {data, {noeol, Result}}} ->
            collect_response(Port, RespAcc, [Result | LineAcc])

    %% Prevent the gen_server from hanging indefinitely in case the
    %% spawned process is taking too long processing the request.
    after 5000 -> 
            timeout
    end.

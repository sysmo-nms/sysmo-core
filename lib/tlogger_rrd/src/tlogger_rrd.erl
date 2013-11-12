-module(tlogger_rrd).
-behaviour(gen_server).
-include_lib("kernel/include/file.hrl").

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
    exec/1
]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

exec(Command) ->
    gen_server:call(?MODULE, {exec, Command}).

init([]) ->
    io:format("self is ~p~n",[self()]),
    P = erlang:open_port(
        {spawn, "/usr/bin/rrdtool -"}, 
        [use_stdio, exit_status, {line, 100}]
    ),
    {ok, P}.

handle_call({exec, Command}, _F, P) ->
    erlang:port_command(P, Command),
    case collect_response(P) of
        {response, Response} -> 
            {reply, Response, P};
        timeout -> 
            {stop, port_timeout, P}
    end.

    %{reply, ok, P}.

handle_cast(_,S) ->
    {noreply, S}.

handle_info(Info, S) ->
    io:format("received info ~p~n",[Info]),
    {noreply, S}.

terminate(_,_) ->
    ok.

code_change(_,S,_) ->
    {ok, S}.

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


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
    gen_server:call(?MODULE, {exec, string:concat(Command, "\n")}).

init([]) ->
    {ok, Exe} =application:get_env(tlogger_rrd, command),
    P = erlang:open_port({spawn, Exe}, 
        [use_stdio, exit_status, {line, 100}]),
    {ok, P}.

handle_call({exec, Command}, _F, P) ->
    erlang:port_command(P, Command),
    Rep = get_response(P),
    {reply, Rep, P}.

handle_cast(_,S) ->
    {noreply, S}.

handle_info(_, S) ->
    {noreply, S}.

terminate(_,_) ->
    ok.

code_change(_,S,_) ->
    {ok, S}.

get_response(Port) ->
    receive
        {Port, {data, {eol, "ERROR: " ++ _}}} ->
            error;
        {Port, {data, {eol, "OK " ++ _}}} ->
            ok;
        {Port, {data, _}} ->
            get_response(Port)
    after 5000 -> 
            timeout
    end.


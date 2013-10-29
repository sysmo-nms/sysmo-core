-module(tlogger_text).
-behaviour(gen_server).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    start_link/1,
    log/2,
    dump/1
]).


start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

log(Pid, Message) ->
    gen_server:cast(Pid, {log, Message}).

dump(Pid) ->
    gen_server:call(Pid, dump).


init(FileName) ->
    file:write_file(FileName, <<>>),
    {ok, FileName}.

handle_call(dump, _, S) ->
    {ok, Bin} = file:read_file(S),
    {reply, Bin, S};
handle_call(_,_,S) ->
    {reply, ok, S}.

handle_cast({log, Message}, S) ->
    file:write_file(S, Message, [append]),
    {noreply, S};
handle_cast(_,S) ->
    {noreply, S}.

handle_info(_, S) ->
    {noreply, S}.

terminate(_,_) ->
    ok.

code_change(_,S,_) ->
    {ok, S}.


-module(template_terminal_logger).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

init(_Args) ->
    {ok, []}.

handle_event(Event, State) ->
    io:format("received ~p~n", [Event]),
    {ok, State}.





%% not used
handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Args, _State) ->
    ok.

code_change(_OldVsn, State, _ExtraA) ->
    {ok, State}.

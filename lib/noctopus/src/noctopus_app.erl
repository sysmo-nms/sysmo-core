% @private
-module(noctopus_app).
-behaviour(application).

-export([
    start/2,
    start_phase/3,
    stop/1
]).

start(_Type, _Args) ->
    noctopus_sup:start_link().

start_phase(_,_,_) -> ok.

stop(_State) ->
    ok.

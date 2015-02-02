% @private
-module(errd_app).
-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_Type, _Args) ->
    errd_sup:start_link().

stop(_State) ->
    ok.

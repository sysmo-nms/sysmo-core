% @private
-module(equartz_app).
-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_Type, _Args) ->
    equartz_sup:start_link().

stop(_State) ->
    ok.

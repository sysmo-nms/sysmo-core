% @private
-module(j_server_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    j_server_sup:start_link().

stop(_State) ->
    ok.

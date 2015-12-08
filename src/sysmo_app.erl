%% @private
-module(sysmo_app).
-behaviour(application).
-include_lib("kernel/include/file.hrl").

-export([start/2, start_phase/3, stop/1]).

start(_Type, _Args) ->
    sysmo_sup:start_link().

start_phase(listen, _Type, _Args) -> supercast:listen().

stop(_State) ->
    ok.

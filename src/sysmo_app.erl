% @private
-module(sysmo_app).
-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_Type, _Args) ->
    write_cookie(),
    sysmo_sup:start_link().

stop(_State) ->
    ok.

write_cookie() ->
    Cookie = erlang:atom_to_list(erlang:get_cookie()),
    file:write_file("cfg/sysmo.cookie", Cookie).

-module(ifs_server).
-behaviour(gen_server).
-include_lib("../include/client_state.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0]).

%% API
% @doc Start the ifs_server
-spec ifs_server:start_link() -> {ok, Pid::pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% @doc callback function for modsrv
-spec ifs_server:modsrv_notify(Any::any()) -> ok.
modsrv_notify(_Args) ->
    ok.

%% gen_server callbacks
init([]) ->
    modsrv:hello({main_ifs, [
        {modsrv_callback, ?MODULE},
        {event_handler, ifs_events}
        ]}),
    {ok, []}.

handle_call(_R, _F, S) ->
    {noreply, S}.

handle_cast(_R, S) ->
    {noreply, S}.

handle_info(_I, S) ->
    {noreply, S}.

terminate(_R, _S) ->
    normal.

code_change(_O, S, _E) ->
    {ok, S}.


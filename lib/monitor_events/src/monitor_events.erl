-module(monitor_events).
-include("../monitor/include/monitor.hrl").
-include_lib("stdlib/include/qlc.hrl").
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
    start_link/0
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, no_state}.

handle_call(_R, _F, S) ->
    {noreply, S}.

% 
handle_cast(_,S) ->
    {noreply, S}.

%
handle_info(_, S) ->
    {noreply, S}.

%
terminate(_,_) ->
    ok.

%
code_change(_,S,_) ->
    {ok, S}.

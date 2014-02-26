-module(locator_query).
-behaviour(gen_server).
-include("include/locator.hrl").
-include("../nocto_snmpm/include/nocto_snmpm.hrl").

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    start_link/1
]).

start_link(Cfg) ->
    gen_server:start_link(?MODULE, Cfg, []).

init(Conf) ->
    ?LOG({started, Conf}),
    {ok, []}.

%  
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

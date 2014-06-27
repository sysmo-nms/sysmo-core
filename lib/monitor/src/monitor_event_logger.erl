% This file is part of "Enms" (http://sourceforge.net/projects/enms/)
% Copyright (C) 2012 <Sébastien Serre sserre.bx@gmail.com>
% 
% Enms is a Network Management System aimed to manage and monitor SNMP
% target, monitor network hosts and services, provide a consistent
% documentation system and tools to help network professionals
% to have a wide perspective of the networks they manage.
% 
% Enms is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
% 
% Enms is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with Enms.  If not, see <http://www.gnu.org/licenses/>.
% @private
-module(monitor_event_logger).
-behaviour(gen_server).
-include("include/monitor.hrl").

-export([
    start_link/0
]).

-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

-export([
    log/1,
    rotate/0
]).

-record(state, {
    dir,
    file
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

rotate() ->
    gen_server:cast(?MODULE, rotate).

log(Event) ->
    gen_server:cast(?MODULE, {log, Event}).



%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% INIT
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
init([]) -> 
    {ok, Dir} = application:get_env(monitor, events_dir),
    gen_server:cast(?MODULE, rotate),
    {ok, #state{dir = Dir}}.
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% HANDLE_CALL 
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
handle_call(_R, _F, S) ->
    {noreply, S}.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% HANDLE_CAST
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
handle_cast({log, Event}, S) ->
    ?LOG({log, Event}),
    {noreply, S};

handle_cast(rotate, S) ->
    {ok, DetsFile, SecondNext} = update_data_file(S#state.dir),
    timer:apply_after(SecondNext * 1000, ?MODULE, rotate, []),
    ?LOG({rotate, calendar:now_to_local_time(erlang:now())}),
    {noreply, S#state{file = DetsFile}};
    
handle_cast(R, S) ->
    error_logger:info_msg(
        "unknown cast for command ~p ~p ~p~n", [?MODULE, ?LINE, R]
    ),
    {noreply, S}.


%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% HANDLE_INFO
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
handle_info(_I, S) ->
    {noreply, S}.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% TERMINATE  
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
terminate(_R, _S) ->
    normal.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% CODE_CHANGE
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
code_change(_O, S, _E) ->
    {ok, S}.



%%
%% PRIVATE
%% 
update_data_file(Dir) ->
    {{Year,Month,Day}, {H,M,S}} = calendar:now_to_local_time(erlang:now()),

    MaxSecDay  = 86400, % (24 hour * 60 * 60)
    CurrentSec = (S + (M * 60) + (H * 60 * 60)),
    SecondNext = MaxSecDay - CurrentSec + 5, % + 1 should start at 0:00:05

    DetsName   = lists:flatten(io_lib:format("~p-~p-~p.dets", [Year,Month,Day])),
    DetsFile   = filename:join(Dir, DetsName),

    {ok, DetsFile, SecondNext}.

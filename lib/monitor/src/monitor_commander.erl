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
-module(monitor_commander).
-behaviour(supercast_commander).
-behaviour(gen_server).
-include("include/monitor.hrl").
-include("include/monitor_snmp.hrl").
-include_lib("kernel/include/file.hrl").

% API
-export([
    start_link/0,
    handle_command/2
]).

% GEN_SERVER
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

-record(state, {
    data_dir,
    check_db_ref
}).

-define(DETS_CHECK_INFO, check_infos_db).

% used to create random target and probe names
% 1 000 000 possible values
-define(RAND_RANGE, 1000000).
% but must be a minimum of 100000
-define(RAND_MIN,   99999).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

handle_command(Command, CState) ->
    {modMonitorPDU, {fromClient, CastCommand}} = Command,
    gen_server:cast(?MODULE, {CastCommand, CState}).

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% INIT
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
init([]) -> 
    random:seed(erlang:now()),
    {ok, DataDir}  = application:get_env(monitor, targets_data_dir),
    {ok, DetsRef}  = init_check_info_database(DataDir),
    State = #state{
        data_dir=DataDir,
        check_db_ref=DetsRef
    },
    {ok, State}.

init_check_info_database(VarDir) ->
    DetsFile     = filename:absname_join(VarDir, "check_infos.dets"),
    case filelib:is_file(DetsFile) of
        true  ->
            ok = file:delete(DetsFile);
        false ->
            ok
    end,
    {ok, N} = dets:open_file(?DETS_CHECK_INFO, [
        {file,   DetsFile},
        {keypos, 1},
        {ram_file, false},
        {auto_save, 180000},
        {type, set}
    ]),
    dets:close(N),
    dets:open_file(DetsFile).

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
handle_cast({{extendedQueryFromClient, 
        {_, QueryId, {snmpElementInfoQuery, Query}}}, CState}, S) ->

    %Perms = monitor_master:get_perms(
    command_net_element_wizard:handle_snmpElementInfoQuery(QueryId, CState, Query),
    {noreply, S};

handle_cast({{extendedQueryFromClient, 
        {_, QueryId, {snmpElementCreateQuery, Query}}}, CState}, S) ->
    command_net_element_wizard:handle_snmpElementCreateQuery(QueryId, CState, Query, S#state.data_dir),
    {noreply, S};


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
terminate(_R, #state{check_db_ref=Ref}) ->
    dets:close(Ref),
    normal.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% CODE_CHANGE
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
code_change(_O, S, _E) ->
    {ok, S}.

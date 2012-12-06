% This file is part of "Enms" (http://sourceforge.net/projects/enms/)
% Copyright (C) 2012 <Sébastien Serre sserre.bx@gmail.com>
% 
% Enms is a Network Management System aimed to manage and monitor SNMP
% targets, monitor network hosts and services, provide a consistent
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
%%% @doc
%%% tcp client interface
%%% @end
-module(tcp_client).
-behaviour(gen_fsm).
-include_lib("../include/client_state.hrl").

-export([start_link/0, set_socket/2, auth_set/2]).
-export([init/1, handle_event/3, handle_sync_event/4, 
            handle_info/3, terminate/3, code_change/4, send/2]).
-export(['WAIT_FOR_SOCKET'/2, 'WAIT_FOR_CLIENT_AUTH'/2, 'RUNNING'/2]).

-define(TIMEOUT, 30000).
-define(MAX_AUTH_ATEMPT, 3).


%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc To be called by the supervisor in order to start the server.
%%	  If init/1 fails with Reason, the function returns {error,Reason}.
%%	  If init/1 returns {stop,Reason} or ignore, the process is
%%	  terminated and the function returns {error,Reason} or ignore,
%%	  respectively.
%% @end
%%-------------------------------------------------------------------------
start_link() ->
	io:format("start_link ~p~n", [?MODULE]),
	gen_fsm:start_link(?MODULE, [], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
	gen_fsm:send_event(Pid, {socket_ready, Socket}).

%auth_set(auth_success,  [Pid, Ref, {UserName, Roles}]) ->
auth_set(auth_success,  NewState) ->
    Pid         = NewState#client_state.pid,
    Ref         = NewState#client_state.ref,
    UserName    = NewState#client_state.user_name,
    Roles       = NewState#client_state.user_roles,
    gen_fsm:send_event(Pid, {auth_success, Ref, UserName, Roles});

%auth_set(auth_fail,     [Pid, Ref, UserName]) ->
auth_set(auth_fail,     NewState) ->
    Pid         = NewState#client_state.pid,
    Ref         = NewState#client_state.ref,
    UserName    = NewState#client_state.user_name,
    gen_fsm:send_event(Pid, {auth_fail, Ref, UserName}).

send(SockState, Pdu) ->
    gen_fsm:send_event(SockState#client_state.pid, 
        {send_pdu, SockState#client_state.ref, Pdu}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}		  |
%%		  {ok, StateName, StateData, Timeout} |
%%		  ignore							  |
%%		  {stop, StopReason}
%% @private
%%-------------------------------------------------------------------------
init([]) ->
	process_flag(trap_exit, true),
	{ok, 'WAIT_FOR_SOCKET', #client_state{
        state           = 'WAIT_FOR_SOCKET'}}.

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}		  |
%%		  {next_state, NextStateName, NextStateData, Timeout} |
%%		  {stop, Reason, NextStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, Socket}, State) when is_port(Socket) ->
	%% Now we own the socket!
	inet:setopts(Socket, [{active, once}, {packet, 2}, binary]),
	{ok, {IP, Port}} = inet:peername(Socket),
    NextState = State#client_state{
        socket          = Socket,
        addr            = IP,
        port            = Port, 
        ref             = make_ref(),
        pid             = self(),
        module          = ?MODULE,
        state           = 'WAIT_FOR_CLIENT_AUTH'},
    ifs_server:notify_connection(NextState),
	{next_state, 'WAIT_FOR_CLIENT_AUTH', NextState, ?TIMEOUT};

'WAIT_FOR_SOCKET'(Other, State) ->
	error_logger:error_msg("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
	{next_state, 'WAIT_FOR_SOCKET', State}.


%%-------------------------------------------------------------------------
%% process user credentials here
%%-------------------------------------------------------------------------
'WAIT_FOR_CLIENT_AUTH'({client_data, AsnData}, State) ->
    ifs_server:handle_pdu(State, AsnData),
	{next_state, 'WAIT_FOR_CLIENT_AUTH', State, ?TIMEOUT};

'WAIT_FOR_CLIENT_AUTH'({auth_success, Ref, User, Roles}, #client_state{ref = Ref} = State) ->
    NextState = State#client_state{
        user_name       = User,
        user_roles      = Roles,
        state           = 'RUNNING'},
    {next_state, 'RUNNING', NextState};

'WAIT_FOR_CLIENT_AUTH'({auth_fail, Ref, User}, #client_state{ref = Ref} = State) ->
	io:format("failed to register with user ~p ~n", [User]),
    {next_state, 'WAIT_FOR_CLIENT_AUTH', State, ?TIMEOUT};

'WAIT_FOR_CLIENT_AUTH'({send_pdu, Ref,  Pdu},  #client_state{ref = Ref} = State) ->
    gen_tcp:send(State#client_state.socket, Pdu),
    {next_state, 'WAIT_FOR_CLIENT_AUTH', State};

'WAIT_FOR_CLIENT_AUTH'(timeout, #client_state{auth_request_count = ?MAX_AUTH_ATEMPT} = State) ->
	{stop, normal, State};

'WAIT_FOR_CLIENT_AUTH'(timeout, State) ->
    NextState = State#client_state{auth_request_count = State#client_state.auth_request_count + 1},
    ifs_server:notify_connection(NextState),
	{next_state, 'WAIT_FOR_CLIENT_AUTH', NextState, ?TIMEOUT};

'WAIT_FOR_CLIENT_AUTH'(Data, State) ->
	io:format("~p Ignoring data: ~p\n", [self(), Data]),
	{next_state, 'WAIT_FOR_CLIENT_AUTH', State}.

%%-------------------------------------------------------------------------
%% application running
%%-------------------------------------------------------------------------
'RUNNING'({client_data, Data}, State) ->
    ifs_server:handle_pdu(State, Data),
	{next_state, 'RUNNING', State};

'RUNNING'({send_pdu, Ref, Pdu}, #client_state{ref = Ref} = State) ->
    gen_tcp:send(State#client_state.socket, Pdu),
    {next_state, 'RUNNING', State};

'RUNNING'(timeout, State) ->
	error_logger:error_msg("~p Running Client connection timeout - closing.\n", [self()]),
	{stop, normal, State};

'RUNNING'(Data, State) ->
	io:format("~p Running Ignoring data: ~p\n", [self(), Data]),
	{next_state, 'RUNNING', State}.

%%-------------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}		  |
%%		  {next_state, NextStateName, NextStateData, Timeout} |
%%		  {stop, Reason, NextStateData}
%% @private
%%-------------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
	io:format("handle_event ~p ~p~n", [?MODULE, StateData]),
	{stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}			|
%%		  {next_state, NextStateName, NextStateData, Timeout}   |
%%		  {reply, Reply, NextStateName, NextStateData}		  |
%%		  {reply, Reply, NextStateName, NextStateData, Timeout} |
%%		  {stop, Reason, NextStateData}						  |
%%		  {stop, Reason, Reply, NextStateData}
%% @private
%%-------------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, StateData) ->
	io:format("handle_sync_event ~p ~p~n", [?MODULE, StateData]),
	{stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}		  |
%%		  {next_state, NextStateName, NextStateData, Timeout} |
%%		  {stop, Reason, NextStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info({tcp,Socket, Bin}, StateName, #client_state{socket=Socket} = StateData) ->
	inet:setopts(Socket, [{active, once}]),
	?MODULE:StateName({client_data, Bin}, StateData);

handle_info({tcp_closed, Socket}, _StateName,
			#client_state{socket=Socket, addr=Addr} = StateData) ->
	error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
	{stop, normal, StateData};

handle_info(Info, StateName, StateData) ->
    io:format("INFO: ~p~n", [Info]),
    io:format("STATENAME: ~p~n", [StateName]),
    io:format("STATEDATA: ~p~n", [StateData]),
	io:format("handle_info 3 ~p ~p ~p~n", [?MODULE, StateData, Info]),
	{noreply, StateName, StateData}.

%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, #client_state{socket=Socket} = State) ->
	io:format("terminate ~p ~p~n", [?MODULE, _Reason]),
	(catch gen_tcp:close(Socket)),
    ifs_server:notify_disconnection(State),
	ok.

%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NextState, NextStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
	io:format("code_change ~p ~p~n", [?MODULE, StateData]),
	{ok, StateName, StateData}.

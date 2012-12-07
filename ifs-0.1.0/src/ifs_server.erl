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
-module(ifs_server).
-behaviour(gen_server).
-include_lib("../include/client_state.hrl").

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
% API
-export([   
        start_link/0, 
        notify_connection/1, 
        notify_disconnection/1, 
        handle_msg/2, 
        register_mod/1]).

-record(if_module, {callback_mod, asnkey}).
-record(if_server_state, {
    authmod,            % authentication module
    modules = []        % list of #if_module
    }).
-define(AUTH_MOD, bifs_auth_local).

%%-----------------------------------------------------
%% API
%%-----------------------------------------------------
% @doc start the server. No arguments.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% @doc register_mod execute par un module pour etre presente au client.
register_mod({ModName, AsnKey}) ->
    gen_server:call(?MODULE, {new_mod, ModName, AsnKey}).
    
% @doc execute par le client lors de sa connection
notify_connection(#client_state{module = Module} = SocketState) ->
    Module:send(SocketState, {modIfPDU, {fromServer, {authReq, ldap}}}).

% @doc execute par le client lors de sa deconnection
notify_disconnection(SocketState) ->
    ifs_mpd:del_client(SocketState).

% @doc handle_msg qui viennent du client
handle_msg(SocketState, {Mod, Msg}) ->
    case Mod of
        modIfPDU ->
            io:format("~p  modIfPDU receive ~p~n", [?MODULE, Msg]),
            process_msg(Msg, SocketState);
        OtherMod ->
            gen_server:call(?MODULE, {ext_msg, OtherMod, Msg, SocketState})
    end.



% @doc handle message destine au server.
% @private
process_msg({fromClient, {authRep, {ldap, {_, UName, UPass}}}}, #client_state{module = Mod} = SocketState) ->

    case ?AUTH_MOD:authenticate(UName, UPass) of
        {ok, Roles} ->
            gen_server:cast(?MODULE, {send_ack, Roles, SocketState, UName});
        fail ->
            Mod:send(SocketState, {modIfPDU, {fromServer, {authError,
                    {'AuthPDU_fromServer_authError', badPass, UName, UPass}}}});
        Other ->
            io:format("unknown ~p in ~p line ~p~n", [Other, ?MODULE, ?LINE])
    end;

process_msg({fromClient, {subscribe, Mods}}, #client_state{state = 'RUNNING'} = SocketState) ->
    lists:foreach(fun(X) -> 
        Mod = erlang:list_to_atom(X),
        Mod:init_client(SocketState),
        ifs_mpd:register_client(SocketState, X) 
    end, Mods);

process_msg({fromClient, {unsubscribe, Mods}}, #client_state{state = 'RUNNING'} = SocketState) ->
    %gen_server:cast(?MODULE, {subscribe, Mods, CState});
    lists:foreach(fun(X) -> ifs_mpd:unregister_client(SocketState, X) end, Mods);

process_msg(A, S) ->
    io:format("ici ~p ~p~n", [A,S]).
    

%%--------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------
init([]) ->
    S = #if_server_state{authmod = ?AUTH_MOD},
    {ok, S}.

% CALL
handle_call({new_mod, ModName, AsnKey}, _From, S) ->
    CurrentMods = S#if_server_state.modules,
    NewMod = #if_module{callback_mod = ModName, asnkey = AsnKey},
    UpdatedMods = [NewMod | CurrentMods],
    {reply, ok, S#if_server_state{modules = UpdatedMods}};

handle_call({ext_msg, ModKey, Msg, SocketState}, _From, S) ->
    case lists:keysearch(ModKey, 3, S#if_server_state.modules) of
        false -> 
            io:format("bad module ~p ~p~n", [?MODULE, ?LINE]);
        {value, {if_module, Mod, ModKey}} ->
            Mod:handle_msg(Msg, SocketState)
    end,
    {reply, ok, S};

handle_call(_R, _F, S) ->
    {reply, ok, S}.

% CAST
handle_cast({send_ack, Roles, 
    #client_state{module = CliMod} = SocketState, UName}, S) ->

    Modules = lists:map(fun(X) -> 
        erlang:atom_to_list(X#if_module.callback_mod) 
    end, S#if_server_state.modules),

    NewSocketState  = SocketState#client_state{user_roles = Roles, user_name = UName},
    CliMod:auth_set(auth_success, NewSocketState),
    CliMod:send(NewSocketState, {modIfPDU, {fromServer, {authAck,
                                {'AuthPDU_fromServer_authAck', Roles, Modules}}}}),
    {noreply, S};

handle_cast(_R, S) ->
    {noreply, S}.

% OTHER
handle_info(_I, S) ->
    {noreply, S}.

terminate(_R, _S) ->
    normal.

code_change(_O, S, _E) ->
    {ok, S}.

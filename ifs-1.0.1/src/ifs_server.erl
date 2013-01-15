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
% @private
-module(ifs_server).
-behaviour(gen_server).
-include_lib("../include/client_state.hrl").

% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

% API
-export([
    start_link/2,
    notify_connection/1,
    notify_disconnection/1,
    handle_msg/2,
    handle_mod_event/2]).

-record(ifs_app_record, {
    app_name,
    callback_mod,
    asnkey}).

-record(if_server_state, {
    authmod,            % authentication module
    modules = []}).     % list of #ifs_app_record

%%-----------------------------------------------------
%% API
%%-----------------------------------------------------
% @doc start the server. No arguments.
start_link(AuthMod, ManagedMods) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, 
                                [AuthMod, ManagedMods], []).

% @doc execute par le client lors de sa connection
notify_connection(#client_state{module = Module} = ClientState) ->
    Module:send(ClientState, {modIfPDU, {fromServer, {authReq, ldap}}}).

% @doc execute par le client lors de sa deconnection
notify_disconnection(ClientState) ->
    ifs_rbac:del_client(ClientState).

% @doc message received from a gen_event listener registered on another module
handle_mod_event(Mod, Event) ->
    gen_server:cast(?MODULE, {handle_mod_event, Mod, Event}).

% @doc handle_msg from client
handle_msg(ClientState, {Mod, Msg}) ->
    case Mod of
        modIfPDU ->
            io:format("~p  modIfPDU receive ~p~n", [?MODULE, Msg]),
            process_msg(Msg, ClientState);
        OtherMod ->
            gen_server:call(?MODULE, {ext_msg, OtherMod, Msg, ClientState})
    end.

% @doc 
% handle PDU decoded message comming from client
% @end
process_msg({fromClient, {authResp, {_, UName, UPass}}},
        #client_state{module = Mod} = ClientState) ->
    AuthResponce = gen_server:call(?MODULE, {auth_rep, UName, UPass}),
    case AuthResponce of
        {ok, Roles} ->
            gen_server:cast(?MODULE, {send_ack, Roles, ClientState, UName});
        fail ->
            Mod:send(ClientState, {modIfPDU, {fromServer, {authError,
                    {'AuthPDU_fromServer_authError', badPass, UName, UPass}}}});
        Other ->
            io:format("unknown ~p in ~p line ~p~n", [Other, ?MODULE, ?LINE])
    end;

process_msg({fromClient, {subscribe, Mods}},
        #client_state{state = 'RUNNING'} = ClientState) ->
    lists:foreach(fun(X) ->
        Mod = erlang:list_to_atom(X),
        ifs_rbac:register_client(ClientState, X),
        gen_server:call(?MODULE, {subscribe_client, ClientState, Mod})
    end, Mods);

process_msg({fromClient, {unsubscribe, Mods}},
        #client_state{state = 'RUNNING'} = ClientState) ->
    %gen_server:cast(?MODULE, {subscribe, Mods, CState});
    lists:foreach(fun(X) ->
        ifs_rbac:unregister_client(ClientState, X) end, Mods);

process_msg(A, S) ->
    io:format("ici ~p ~p~n", [A,S]).

%%--------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------
init([AuthMod, ManagedMods]) ->
    {ok, #if_server_state{
        authmod = AuthMod,
        modules = ManagedMods}}.

% CALL
% @doc Auth responce from the client
handle_call({auth_rep, UName, UPass}, _From,
        #if_server_state{authmod = AuthMod} = S) ->
    Rep = AuthMod:authenticate(UName, UPass),
    {reply, Rep, S};

% @doc handle message destined to another module
handle_call({ext_msg, ModKey, Msg, ClientState}, _From, S) ->
    case lists:keysearch(ModKey, 3, S#if_server_state.modules) of
        false ->
            io:format("bad module ~p ~p~n", [?MODULE, ?LINE]);
        {value, {ifs_app_record, Mod, ModKey}} ->
            Mod:handle_msg(Msg, ClientState)
    end,
    {reply, ok, S};

% @doc 
% Subscribe client to a specific module and 
% call a ModuleCallback:initial_conn/1
% @end
handle_call({subscribe_client, 
        #client_state{module = Mod} = ClientState, Module}, _From, 
        #if_server_state{modules = Modules} = S) ->
    %Modules = S#if_server_state.modules,
    {value, ModuleRecord} = lists:keysearch(Module, 2, Modules),
    ModuleCallback = ModuleRecord#ifs_app_record.callback_mod,
    case ModuleCallback:initial_conn(ClientState) of
        ok -> Mod:send(ClientState, 
                {modIfPDU, {fromServer, 
                    {subscribeOk, erlang:atom_to_list(Module)}}});
        _O -> io:format("error ~p ~p ~p~n", [?MODULE, ?LINE, _O])
    end,
    {reply, ok, S};

handle_call(_R, _F, S) ->
    {reply, ok, S}.

% CAST
% @doc Send an Ack PDU to the client defined by ClientState
handle_cast({send_ack, Roles,
        #client_state{module = CliMod} = ClientState, Name}, S) ->
    Modules = lists:map(fun(X) -> erlang:atom_to_list(X#ifs_app_record.app_name)
        end, S#if_server_state.modules),
    CliMod:auth_set(success, ClientState, Name, Roles, Modules),
    CliMod:send(ClientState, {modIfPDU, {fromServer, {authAck,
        {'AuthPDU_fromServer_authAck', Roles, Modules}}}}),
    {noreply, S};

% @doc 
handle_cast({handle_mod_event, Mod, Event}, S) ->
    spawn(fun() ->
        Modules = S#if_server_state.modules,
        {value, ModRecord} = lists:keysearch(Mod, 2, Modules),
        CallbackMod = ModRecord#ifs_app_record.callback_mod,
        case CallbackMod:pre_process(Event) of
            {Roles, AsnMsg} ->
                ifs_rbac:handle_event(Mod, AsnMsg, Roles);
            ok ->
                true;
            Other ->
                io:format("Error: ~p ~p ~p~n", [?MODULE,?LINE,Other])
        end
    end),
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

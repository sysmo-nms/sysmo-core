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
    register_mod/1,
    handle_mod_event/2,
    modsrv_notify/1]).

-record(if_module, {
    mod_name,
    callback_mod,
    asnkey}).

-record(if_server_state, {
    authmod,            % authentication module
    modules = []}).     % list of #if_module

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
notify_connection(#client_state{module = Module} = ClientState) ->
    Module:send(ClientState, {modIfPDU, {fromServer, {authReq, ldap}}}).

% @doc execute par le client lors de sa deconnection
notify_disconnection(ClientState) ->
    ifs_rbac:del_client(ClientState).

% @doc message received from a gen_event registered on another module
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

% @doc handle message destine au server.
process_msg({fromClient, {authRep, {ldap, {_, UName, UPass}}}}, #client_state{module = Mod} = ClientState) ->

    case ?AUTH_MOD:authenticate(UName, UPass) of
        {ok, Roles} ->
            gen_server:cast(?MODULE, {send_ack, Roles, ClientState, UName});
        fail ->
            Mod:send(ClientState, {modIfPDU, {fromServer, {authError,
                    {'AuthPDU_fromServer_authError', badPass, UName, UPass}}}});
        Other ->
            io:format("unknown ~p in ~p line ~p~n", [Other, ?MODULE, ?LINE])
    end;

process_msg({fromClient, {subscribe, Mods}}, #client_state{state = 'RUNNING'} = ClientState) ->
    lists:foreach(fun(X) -> 
        Mod = erlang:list_to_atom(X),
        ifs_rbac:register_client(ClientState, X),
        gen_server:call(?MODULE, {subscribe_client, ClientState, Mod})
    end, Mods);

process_msg({fromClient, {unsubscribe, Mods}}, #client_state{state = 'RUNNING'} = ClientState) ->
    %gen_server:cast(?MODULE, {subscribe, Mods, CState});
    lists:foreach(fun(X) -> ifs_rbac:unregister_client(ClientState, X) end, Mods);

process_msg(A, S) ->
    io:format("ici ~p ~p~n", [A,S]).
    
% @doc modsrv callback
-spec ifs_server:modsrv_notify(Args::list()) -> ok.
modsrv_notify(Args) ->
    gen_server:call(?MODULE, {modsrv_event, Args}).

%%--------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------
init([]) ->
    % notify mdsrv of the availability of ?MODULE,
    modsrv:hello({main_ifs, [
        {modsrv_callback, ?MODULE},
        {event_handler, ifs_events}]}),
    {ok, #if_server_state{authmod = ?AUTH_MOD}}.

% CALL
handle_call({new_mod, ModName, AsnKey}, _From, S) ->
    CurrentMods = S#if_server_state.modules,
    NewMod = #if_module{callback_mod = ModName, asnkey = AsnKey},
    UpdatedMods = [NewMod | CurrentMods],
    {reply, ok, S#if_server_state{modules = UpdatedMods}};

handle_call({modsrv_event, {ModName, Args}}, _From, S) ->
    CurrentMods = S#if_server_state.modules,
    io:format("~p modsrv_event ~p ~n", [?MODULE, Args]),
    case lists:keysearch(main_ifs, 1, Args) of
        false -> {reply, ok, S};
        {value, {_, Opts}} ->
            {value, {_, CallBack}}  = lists:keysearch(callback, 1, Opts),
            {value, {_, AsnKey}}    = lists:keysearch(asnkey, 1, Opts),
            NewMod = #if_module{mod_name = ModName, callback_mod = CallBack, asnkey = AsnKey},
            case lists:keysearch(listen_events, 1, Opts) of
                false -> ok;
                {value, {_, true}}  ->
                    {value, {_, EventHandler}} = lists:keysearch(event_handler, 1, Args),
                    gen_event:add_handler(EventHandler, ifs_mod_listener, ModName)
            end,
            {reply, ok, S#if_server_state{modules = [NewMod | CurrentMods]}}
    end;

handle_call({ext_msg, ModKey, Msg, ClientState}, _From, S) ->
    case lists:keysearch(ModKey, 3, S#if_server_state.modules) of
        false -> 
            io:format("bad module ~p ~p~n", [?MODULE, ?LINE]);
        {value, {if_module, Mod, ModKey}} ->
            Mod:handle_msg(Msg, ClientState)
    end,
    {reply, ok, S};

handle_call({subscribe_client, ClientState, Module}, _From, S) ->
    io:format("-<<<<<<<<<<<<<<<<modname is ~p state is ~p~n", [Module, S]),
    Modules = S#if_server_state.modules,
    {value, ModuleRecord} = lists:keysearch(Module, 2, Modules),
    ModuleCallback = ModuleRecord#if_module.callback_mod,
    ModuleCallback:initial_conn(ClientState),
    {reply, ok, S};

handle_call(_R, _F, S) ->
    {reply, ok, S}.

% CAST
handle_cast({send_ack, Roles, #client_state{module = CliMod} = ClientState, Name}, S) ->
    Modules = lists:map(fun(X) -> erlang:atom_to_list(X#if_module.mod_name) 
        end, S#if_server_state.modules),
    CliMod:auth_set(success, ClientState, Name, Roles, Modules),
    CliMod:send(ClientState, {modIfPDU, {fromServer, {authAck, 
        {'AuthPDU_fromServer_authAck', Roles, Modules}}}}),
    {noreply, S};

handle_cast({handle_mod_event, Mod, Event}, S) ->
    spawn(fun() ->
        Modules = S#if_server_state.modules,
        {value, ModRecord} = lists:keysearch(Mod, 2, Modules),
        CallbackMod = ModRecord#if_module.callback_mod,
        {Roles, AsnMsg} = CallbackMod:pre_process(Event),
        ifs_rbac:handle_event(Mod, AsnMsg, Roles)
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

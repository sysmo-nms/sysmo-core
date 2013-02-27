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
-include("../include/ifs.hrl").

-export([
    init/1,
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

-export([
    start_link/1,
    which_modules/0,
    client_msg/2,
    dump/0
]).

-record(state, {
    modules,
    auth_mod,
    acctrl_mod
}).
 

%%-------------------------------------------------------------
%% API
%%-------------------------------------------------------------
% @private
start_link(ServerConf) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ServerConf, []).


% API FROM MODULES
which_modules() ->
    gen_server:call(?MODULE, {get, modules_name}).

which_auth() ->
    gen_server:call(?MODULE, {get, auth_mod}).


% API FROM CLIENTS
client_msg(Msg, ClientState) ->
    io:format("RECEIVED: ~p~n", [Msg]),
    handle_client_msg(Msg, ClientState).

% DEBUG
% @private
dump() ->
    gen_server:call(?MODULE, dump).

%%-------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%-------------------------------------------------------------
% @private
init({AuthModule, AcctrlMod, IfsModules}) ->
    Ret = lists:foldl(fun({
            ModName,
            Callback, 
            AsnKey, 
            StaticChan, 
            Perm}, ModList) ->
        [
            #ifs_module{
                name            = ModName,
                callback        = Callback, 
                asnkey          = AsnKey, 
                static_chan     = StaticChan,
                perm            = Perm
            }  | ModList
        ]
    end, [], IfsModules),
    {ok, #state{
            modules     = Ret,
            auth_mod    = AuthModule,
            acctrl_mod  = AcctrlMod
        }
    }.

% @private
handle_call(dump, _F, S) ->
    {reply, S, S};

handle_call({get, modules_name}, _F, #state{modules = Mods} = S) ->
    ModNames = lists:foldl(fun(#ifs_module{name = Name}, ModList) ->
        [Name | ModList]
    end, [], Mods),
    {reply, ModNames, S};

handle_call({get, auth_mod}, _F, #state{auth_mod = AuthMod} = S) ->
    {reply, AuthMod, S};

% CLIENT RELATED CALLS
handle_call(_R, _F, S) ->
    io:format("handle_call ~p~p~n", [?MODULE, _R]),
    {noreply, S}.

% CAST
% @private
handle_cast(_R, S) ->
    io:format("handle_cast ~p~p~n", [?MODULE, _R]),
    {noreply, S}.

% OTHER
% @private
handle_info(_I, S) ->
    {noreply, S}.

% @private
terminate(_R, _S) ->
    normal.

% @private
code_change(_O, S, _E) ->
    {ok, S}.

% CLIENT CALL
% @private
handle_client_msg(connect, #client_state{module = Module} = ClientState) ->
    Module:send(ClientState, pdu(authReq, ldap));

handle_client_msg(disconnect, ClientState) ->
    lists:foreach(fun(Mod) ->
        gen_server:call(?MODULE, {unsubscribe, Mod, ClientState}) 
    end, which_modules());

handle_client_msg(
        {message, 
            {modIfPDU, 
                {fromClient,
                    {authResp,
                        {'AuthResp', 
                            Name,
                            Password
        }   }   }   }   },  #client_state{module = CMod} = ClientState) ->
    AuthMod = which_auth(),
    case AuthMod:authenticate(Name, Password) of
        {ok, Groups} ->
            AllowedChans = [],
            CMod:auth_set(success, ClientState, Name, Groups, AllowedChans),
            CMod:send(ClientState,pdu(authAck, {Groups, AllowedChans}));
        fail    ->
            CMod:send(ClientState, pdu(authErr, {Name, Password}))
    end;

handle_client_msg(
        {message, 
            {modIfPDU, 
                {fromClient,
                    {
                        Other
        }   }   }   }, _) ->
    io:format("received ~p~n", [Other]).


% server PDUs
% @private
pdu(authReq, Type) ->
    {modIfPDU, 
        {fromServer, 
            {authReq, 
                Type }}};

pdu(authAck, {Groups, StaticChans}) ->
    StaticChansAsString = lists:foldl(fun(Atom, Accum) ->
        [{'ChanInfo', atom_to_list(Atom), create} | Accum]
    end, [], StaticChans),
    {modIfPDU, 
        {fromServer,
            {authAck,
                {'AuthAck',
                    Groups,
                    StaticChansAsString }}}};

pdu(authErr, {Name, Password}) ->
    {modIfPDU,
        {fromServer,
            {authError,
                {'AuthError',
                    badPass,
                    Name,
                    Password }}}};

pdu(staticSubscribeOk, StaticChan) ->
    {modIfPDU, 
        {fromServer, 
            {staticSubscribeOk, 
                StaticChan }}};

pdu(staticSubscribeErr, StaticChan) ->
    {modIfPDU, 
        {fromServer, 
            {staticSubscribeErr, 
                StaticChan }}};

pdu(subscribeOk, Module) ->
    {modIfPDU, 
        {fromServer, 
            {subscribeOk, 
                Module }}};

pdu(subscribeErr, Module) ->
    {modIfPDU, 
        {fromServer,
            {subscribeErr, 
                Module }}};

pdu(unsubscribeOk, Module) ->
    {modIfPDU, 
        {fromServer, 
            {unsubscribeOk, 
                Module }}};

pdu(unsubscribeErr, Module) ->
    {modIfPDU, 
        {fromServer, 
            {unsubscribeErr, 
                Module }}};

pdu(chanInfo, {Module, Channel, Type}) ->
    {modIfPDU,
        {fromServer,
            {chanInfo,
                {'ChanInfo',
                    Module,
                    Channel,
                    Type }}}}.

% static_subscribe(StaticChan, ClientState) ->
%     case gen_server:call(?MODULE, 
%             {static_subscribe, StaticChan, ClientState}) of
%         true ->
%             Ret = ifs_mpd:client_subscribtion(StaticChan, ClientState),
%             Ret;
%         false -> false
%     end.



% static_chan_filter(Roles, Mods) ->
%     static_chan_filter(Roles, Mods, []).
% 
% static_chan_filter(_, [], Result) ->
%     Result;
% static_chan_filter(Roles, [Mod |Mods], Result) ->
%     case lists:any(fun(Role) ->
%             lists:member(Role, Mod#ifs_module.perm) end, Roles) of
%         true  ->
%             static_chan_filter(Roles, Mods, 
%                 [Mod#ifs_module.static_chan | Result]);
%         false ->
%             static_chan_filter(Roles, Mods, Result)
%     end.

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
-module(ifs_mpd).
-behaviour(gen_server).
-include_lib("../include/client_state.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, handle_event/3, register_client/2, unregister_client/2, del_client/1, dump/0]).

%%-------------------------------------------------------------
%% API
%%-------------------------------------------------------------
% @doc start the server. No arguments.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% @doc Ajoute un client au service si il n'y est pas deja et sourcris au module Mod
register_client(ClientState, Mod) ->
    gen_server:cast(?MODULE, {register_client, ClientState, Mod}).

% @doc retire un client du module Mod
unregister_client(CState, Mod) ->
    gen_server:cast(?MODULE, {unregister_client, CState, Mod}).
    
% @doc delete un client de tous les modules
del_client(CState) ->
    gen_server:cast(?MODULE, {del_client, CState}).

% @doc FromMod doit etre l atome avec lequel s'est enregistre le module via ifs_server:regiter_mod/1.
handle_event(FromMod, Data, Perm) ->
    gen_server:cast(?MODULE, {process_msg, FromMod, Data, Perm}).

% @doc debug function
dump() ->
    gen_server:call(?MODULE, dump).
%%-------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%-------------------------------------------------------------
init([]) ->
    TableID = ets:new(ifs, [set]),
    {ok, TableID}.

handle_call(dump, _F, S) ->
    io:format("~n~nDUMP DUMP DUMP:~n ~p~n~n", [ets:match(S, '$1')]),
    {reply, ok, S};

handle_call(_R, _F, S) ->
    {noreply, S}.

% CAST
handle_cast({register_client, CState, Mod}, S) ->
    ifs_register_client(erlang:list_to_atom(Mod), S, CState),
    {noreply, S};

handle_cast({unregister_client, CState, Mod}, S) ->
    io:format("ifs unregister ~p ~p ~p", [CState, Mod, S]),
    ifs_unregister_client(erlang:list_to_atom(Mod), S, CState),
    {noreply, S};

handle_cast({del_client, CState}, S) ->
    ifs_del_client(S, CState),
    {noreply, S};

handle_cast({process_msg, Mod, Data, Perm}, S) ->
    ifs_process_msg(Mod, Perm, Data, S),
    {noreply, S};

handle_cast(_R, S) ->
    io:format("handle_cast ~p~p~n", [?MODULE, _R]),
    {noreply, S}.

% OTHER
handle_info(_I, S) ->
    {noreply, S}.

terminate(_R, _S) ->
    normal.

code_change(_O, S, _E) ->
    {ok, S}.


%%-------------------------------------------------------------
%% PRIVATE
%%-------------------------------------------------------------
% @private
% @doc cherche dans ets "S" le couple {Mod, Perm} de [Perms]. Si il y a des clients, fait un send_msg
ifs_process_msg(Mod, Perms, Data, S) ->
    Acu = lists:foldl(fun(X, Acc) ->
        A = ets:match(S, {{Mod, X}, '$1'}),
        [A | Acc]
    end, [], Perms),
    case lists:flatten(Acu) of
        []      -> nothing;
        Clients -> 
            {ok, Pdu} = 'NmsPDU':encode('PDU', Data),
            lists:foreach(fun(X) -> 
                SMod = X#client_state.module,
                SMod:send(X, Pdu)
            end, Clients)
    end.

% @private
% @doc ajoute le client aux couples {Mod, role} les crees si ils n existent pas encore
ifs_register_client(Mod, S, CState) ->
    lists:foreach(fun(Role) ->
        case ets:lookup(S, {Mod, Role}) of
            []          -> ets:insert(S, {{Mod, Role}, [CState]});
            [{_,L}]     -> 
                case lists:member(CState, L) of
                    true -> io:format("allready registered~n");
                    false -> 
                        io:format("not registered ~p?~n", [L]),
                        ets:insert(S, {{Mod, Role}, [CState |L]})
                end
        end
    end, CState#client_state.user_roles).
   
% @private
% @doc retire l'utilisateur des couples {Mod, Roles}
ifs_unregister_client(Mod, S, CState) ->
    AllSets = ets:match(S, '$1'),
    All     = lists:map(fun(X) -> [{Key, _}] = X, Key end, AllSets),
    All2 = lists:map(fun({Element,_}) -> 
        case Element of
            Mod -> true;
            _   -> false
        end
    end, All),
    lists:foreach(fun(X) ->
        [{Key, List}] = ets:lookup(S, X),
        NewList = lists:delete(CState, List),
        ets:update_element(S, Key, {2, NewList})
    end, All2).
    
% @private
% @doc retire l'utilisateur de tous les couples {module, roles}
ifs_del_client(S, CState) ->
    AllSets = ets:match(S, '$1'),
    All = lists:map(fun(X) -> [{Key, _}] = X, Key end, AllSets),
    lists:foreach(fun(X) ->
        [{Key, List}] = ets:lookup(S, X),
        NewList = lists:delete(CState, List),
        ets:update_element(S, Key, {2, NewList})
    end, All).

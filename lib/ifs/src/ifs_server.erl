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
    create_chan/3,
    update_chan/3,
    delete_chan/2,
    which_modules/0,
    which_chans/1,
    client_msg/2,
    dump/0
]).

-record(state, {
    modules,
    auth_mod
}).
    
    
%%-------------------------------------------------------------
%% API
%%-------------------------------------------------------------
% @private
start_link(ServerConf) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ServerConf, []).

% API FROM MODULES
create_chan(Module, ChanId, Perm) ->
    gen_server:call(?MODULE, {chan_create, Module, ChanId, Perm}).

delete_chan(Module, ChanId) ->
    gen_server:call(?MODULE, {chan_delete, Module, ChanId}).

update_chan(Module, ChanId, Perm) ->
    gen_server:call(?MODULE, {chan_update, Module, ChanId, Perm}).

which_modules() ->
    gen_server:call(?MODULE, which_modules).

which_chans(Module) ->
    gen_server:call(?MODULE, {which_chans, Module}).

which_auth() ->
    gen_server:call(?MODULE, {get, auth_mod}).
    
% API FROM CLIENTS
client_msg(Msg, ClientState) ->
    handle_client_msg(Msg, ClientState).

% DEBUG
% @private
dump() ->
    gen_server:call(?MODULE, dump).

%%-------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%-------------------------------------------------------------
% @private
init({AuthModule, IfsModules}) ->
    Ret = lists:foldl(fun({ModName, Callback, AsnKey}, ModList) ->
        [
            #ifs_module{
                name            = ModName,
                callback        = Callback, 
                asnkey          = AsnKey, 
                chans           = [],
                subscribers     = []
            }  | ModList
        ]
    end, [], IfsModules),
    {ok, #state{modules = Ret, auth_mod = AuthModule}}.

% @private
handle_call(dump, _F, S) ->
    {reply, S, S};

handle_call({chan_create, Module, ChanId, Perm}, _F, S) ->
    NewState = chan_create(Module, ChanId, Perm, S),
    {reply, ok, NewState};

handle_call({chan_update, Module, ChanId, Perm}, _F, S) ->
    NewState = chan_update(Module, ChanId, Perm, S),
    {reply, ok, NewState};

handle_call({chan_delete, Module, ChanId}, _F, S) ->
    NewState = chan_delete(Module, ChanId, S),
    {reply, ok, NewState};

handle_call(which_modules, _F, #state{modules = Mods} = S) ->
    ModNames = lists:foldl(fun({ifs_module, Name, _, _, _, _}, AppList) ->
        [Name | AppList]
    end, [], Mods),
    {reply, ModNames, S};

handle_call({which_chans, Module}, _F, #state{modules = Mods} = S) ->
    Mod     = lists:keyfind(Module, 2, Mods),
    ChanIds = lists:foldl(fun({chan, Id, _}, ChanList) ->
        [Id | ChanList]
    end, [], Mod#ifs_module.chans),
    {reply, ChanIds, S};

handle_call({get, auth_mod}, _F, #state{auth_mod = AuthMod} = S) ->
    {reply, AuthMod, S};

% CLIENT RELATED CALLS
handle_call({subscribe, _Mod, #client_state{module = _CMod} = _ClientState}, _F, 
        #state{modules = _Modules, auth_mod = _AuthMod} = S) ->
    {reply, ok, S };

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

% @private
chan_create(Module, ChanId, Perm, #state{modules = Mods} = S) ->
    % find the record in the modules list:
    Mod = lists:keyfind(Module, 2, Mods),
    % add in his chans record the new chanid and perms
    NewMod = Mod#ifs_module{
        chans = [#chan{id = ChanId, perm = Perm} | Mod#ifs_module.chans]},
    % replace the modulerecord in the modules list:
    NewModsList = lists:keyreplace(Module, 2, Mods, NewMod),
    % create and return the new state:
    S#state{modules = NewModsList}.

% @private
chan_update(Module, ChanId, Perm, #state{modules = Mods} = S) ->
    % find the Module record in the state:
    Mod = lists:keyfind(Module, 2, Mods),
    % get the chan list
    ChanList = Mod#ifs_module.chans,
    % replace:
    NewChanList = lists:keyreplace(ChanId, 2, ChanList, #chan{
        id = ChanId,
        perm = Perm
    }),
    % create new ifs_module record,
    NewMod = Mod#ifs_module{chans = NewChanList},
    % replace in the state.modules
    NewMods = lists:keyreplace(Module, 2, Mods, NewMod),
    % return the new state
    S#state{modules = NewMods}.

% @private
chan_delete(Module, ChanId, #state{modules = Mods} = S) ->
    % find the Module record in the state:
    Mod = lists:keyfind(Module, 2, Mods),
    % delete the chanid from the ifs_module.chans record:
    NewChans = lists:keydelete(ChanId, 2, Mod#ifs_module.chans),
    % create a new module record,
    NewMod = Mod#ifs_module{chans = NewChans},
    % replace in the state.modules
    NewMods = lists:keyreplace(Module, 2, Mods, NewMod),
    % return the new state
    S#state{modules = NewMods}.

% @private
% subscribe_to_module(#client_state{module = Mod} = ClientState, 
%         Module, ModuleList, AuthMod) ->
%     % take the module record:
%     ModRecord = lists:keyfind(Module, 2, ModuleList),
%     % create an updated one with the #client_state{} added:
%     NewModRecord = ModRecord#ifs_module{subscribers = 
%         [ClientState | ModRecord#ifs_module.subscribers]},
%     % replace from the original list. This will be the return
%     NewModList = lists:keyreplace(Module, 2, ModuleList, NewModRecord),
%     % notify the client of the successfull subscribe. Spawn because
%     % a anging tcp connexion will make the server block:
%     spawn(fun() -> 
%         Mod:send(ClientState, pdu(subscribeOk, {Module}))
%     end),
%     % notify the client of the allready existing chans,
%     % after filtering via the beha_ifs_acctrl module.
%     % spawn() for the same reason as before.
%     lists:foreach(fun(#chan{perm = Perm, id = Id}) ->
%         case AuthMod:satisfy(read, [ClientState], Perm) of
%             []            -> 
%                 ok;
%             [ClientState] -> 
%                 spawn(fun() ->
%                     Mod:send(ClientState, pdu(newChan, {Module, Id}))
%                 end)
%         end
%     end, NewModRecord#ifs_module.chans),
%     NewModList.

% CLIENT CALL
% @private
handle_client_msg(connect, #client_state{module = Module} = ClientState) ->
    io:format("~p receive connect~n", [?MODULE]),
    Module:send(ClientState, pdu(ifsAuth, ldap));

handle_client_msg(disconnect, _ClientState) ->
    io:format("~p receive disconnect~n", [?MODULE]);

handle_client_msg(
        {message, 
            {modIfPDU, 
                {fromClient,
                    {authResp,
                        {'IfPDU_fromClient_authResp', 
                            Name,
                            Password
        }   }   }   }   },  #client_state{module = Module} = ClientState) ->
    AuthMod = which_auth(),
    case AuthMod:authenticate(Name, Password) of
        {ok, Term}  ->
            % get the ifs_modules availables from the server
            Mods = which_modules(),
            % set the new state on the client interface
            io:format("ok loggggggggged!!!!!!!!!!!!!~n"),
            Module:auth_set(success, ClientState, Name, Term, a_to_s(Mods)),
            % send an authAck with the list of available mods
            Module:send(ClientState, pdu(authAck, {Term, a_to_s(Mods)}));
        fail        ->
            % send a auth error
            Module:send(ClientState, pdu(authErr, {Name, Password}))
    end;

handle_client_msg(
        {message,
            {modIfPDU,
                {fromClient,
                    {subscribe,
                        Module
        }   }   }   },  ClientState) ->
    case gen_server:call(?MODULE, 
            {subscribe, list_to_atom(Module), ClientState}) of
        ok ->
            io:format("ok subscribe"),
            ok;
        Other ->
            io:format("other ~p ~p~n",[?LINE, Other])
    end;


handle_client_msg(Any, _ClientState) ->
    io:format("-----------~p~n", [Any]).

% server PDUs
% @private
pdu(ifsAuth, Type) ->
    {modIfPDU, 
        {fromServer, 
            {authReq, 
                Type }}};

pdu(authAck, {Term, Modules}) ->
    {modIfPDU, 
        {fromServer,
            {authAck,
                {'AuthPDU_fromServer_authAck',
                    Term,
                    Modules }}}};

pdu(authErr, {Name, Password}) ->
    {modIfPDU,
        {fromServer,
            {authError,
                {'AuthPDU_fromServer_authError',
                    badPass,
                    Name,
                    Password }}}};

pdu(chanCreate, {Mod, ChanId}) ->
    {modIfPDU,
        {fromServer,
            {chanEvent,
                {newChan,
                Mod,
                ChanId }}}}.

% UTILS
a_to_s(AtomList) ->
    lists:foldl(fun(Atom, Acc) ->
        [atom_to_list(Atom) | Acc]
    end, [], AtomList).

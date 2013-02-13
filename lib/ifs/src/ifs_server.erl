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
    {ok, #state{
        modules     = Ret,
        auth_mod    = AuthModule,
        acctrl_mod  = AcctrlMod
        }
    }.

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
handle_call({subscribe, Mod, ClientState},_F,#state{modules = Modules} = S) ->
    case client_subscribtion_attempt(Mod, Modules, ClientState) of
        {ok, NewModuleList} ->
            {reply, ok, S#state{modules = NewModuleList}};
        false -> 
            {reply, fail, S}
    end;

handle_call({unsubscribe, Mod, ClientState}, _F,
        #state{modules = Modules} = S) ->
    case client_unsubscribtion_attempt(Mod, Modules, ClientState) of
        {ok, NewModuleList} ->
            {reply, ok, S#state{modules = NewModuleList}};
        false -> 
            {reply, fail, S}
    end;

handle_call({subscribe_chan, Mod, Chan, ClientState}, _F, 
        #state{modules = Modules, acctrl_mod = AcctrlMod} = S) ->
    case chan_exist({Mod, Chan}, Modules) of
        true ->
            IfsMod  = lists:keyfind(Mod, 2, Modules),
            IfsChan = lists:keyfind(Chan, 2, IfsMod#ifs_module.chans),
            case AcctrlMod:satisfy(read, [ClientState], IfsChan#chan.perm) of
                {ok, [ClientState]} ->
                    ifs_mpd:new_subscriber(
                        Mod, Chan, IfsMod#ifs_module.callback, ClientState),
                    {reply, ok, S};
                {ok, []} ->
                    {reply, error, S}
            end;
        false ->
            {reply, error, S}
    end;

handle_call({unsubscribe_chan, Mod, Chan, ClientState}, _F, 
        #state{modules = Modules} = S) ->
    case chan_exist({Mod, Chan}, Modules) of
        true ->
            ifs_mpd:del_subscriber(Mod, Chan, ClientState),
            {reply, ok, S};
        false ->
            {reply, error, S}
    end;

handle_call({dump_chans, Mod, #client_state{module = CMod} = ClientState}, _F,
        #state{modules = Modules} = S) ->
    ModRec = lists:keyfind(Mod, 2, Modules),
    lists:foreach(fun(Chan) ->
        ChanId = atom_to_list(Chan#chan.id),
        spawn(fun() ->
            CMod:send(ClientState, pdu(chanInfo, 
                    {atom_to_list(Mod), ChanId, create}))
        end)
    end, ModRec#ifs_module.chans),
    {reply, ok, S};


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
chan_create(ModuleName, ChanId, Perm, #state{modules = Mods} = S) ->
    % find the record in the modules list:
    Mod = lists:keyfind(ModuleName, 2, Mods),
    % add in his chans record the new chanid and perms
    NewMod = Mod#ifs_module{
        chans = [#chan{id = ChanId, perm = Perm} | Mod#ifs_module.chans]},
    % replace the modulerecord in the modules list:
    NewModsList = lists:keyreplace(ModuleName, 2, Mods, NewMod),
    % notify clients of the event
    client_broadcast(Mod#ifs_module.subscribers, 
            pdu(chanInfo, {ModuleName, ChanId, create})),
    % create and return the new state:
    S#state{modules = NewModsList}.

% @private
chan_update(ModuleName, ChanId, Perm, #state{modules = Mods} = S) ->
    % find the Module record in the state:
    Mod = lists:keyfind(ModuleName, 2, Mods),
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
    NewMods = lists:keyreplace(ModuleName, 2, Mods, NewMod),
    % return the new state
    S#state{modules = NewMods}.

% @private
chan_delete(ModuleName, ChanId, #state{modules = Mods} = S) ->
    % find the Module record in the state:
    Mod = lists:keyfind(ModuleName, 2, Mods),
    % delete the chanid from the ifs_module.chans record:
    NewChans = lists:keydelete(ChanId, 2, Mod#ifs_module.chans),
    % create a new module record,
    NewMod = Mod#ifs_module{chans = NewChans},
    % replace in the state.modules
    NewMods = lists:keyreplace(ModuleName, 2, Mods, NewMod),
    % notify clients of the event
    client_broadcast(Mod#ifs_module.subscribers, 
            pdu(chanInfo, {ModuleName, ChanId, delete})),
    % return the new state
    S#state{modules = NewMods}.

% @private
client_subscribtion_attempt(Mod, ModRecordList, ClientState) ->
    % check if the module exist:
    case lists:keyfind(Mod, 2, ModRecordList) of
        false       -> false;
        ModRecord   ->
            % verify that the client is not allready registered:
            Subscribers = ModRecord#ifs_module.subscribers,
            case client_exist_in_list(ClientState, Subscribers) of
                true    -> false;
                false   ->
                    % add the clientstate in #ifs_module.subscribers record
                    NewModRecord = ModRecord#ifs_module{
                        subscribers = [ClientState | Subscribers]
                    },
                    % replace the module in the original list
                    Ret = lists:keyreplace(Mod,2,ModRecordList,NewModRecord),
                    {ok, Ret}
            end
    end.
            
client_unsubscribtion_attempt(Mod, ModRecordList, ClientState) ->
    case lists:keyfind(Mod, 2, ModRecordList) of
        false       -> false;
        ModRecord   ->
            ClientList = ModRecord#ifs_module.subscribers,
            NewClientList = lists:filter(fun(Client) ->
                case are_the_same_client(ClientState, Client) of
                    true -> false;
                    false -> true
                end
            end, ClientList),
            NewModRecord = ModRecord#ifs_module{subscribers = NewClientList},
            NewModList = lists:keyreplace(Mod,2,ModRecordList,NewModRecord),
            {ok, NewModList}
    end.

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
        }   }   }   },  #client_state{module = CMod} = ClientState) ->
    case gen_server:call(?MODULE, 
            {subscribe, list_to_atom(Module), ClientState}) of
        ok  ->
            CMod:send(ClientState, pdu(subscribeOk, Module)),
            gen_server:call(?MODULE, 
                    {dump_chans, list_to_atom(Module), ClientState});
        _   ->
            CMod:send(ClientState, pdu(subscribeErr, Module))
    end;

handle_client_msg(
        {message,
            {modIfPDU,
                {fromClient,
                    {unsubscribe,
                        Module
        }   }   }   },  #client_state{module = CMod} = ClientState) ->
    case gen_server:call(?MODULE, 
            {unsubscribe, list_to_atom(Module), ClientState}) of
        ok  ->
            CMod:send(ClientState, pdu(unsubscribeOk, Module));
        _   ->
            CMod:send(ClientState, pdu(unsubscribeErr, Module))
    end;

handle_client_msg(
        {message,
            {modIfPDU,
                {fromClient,
                    {subscribeChan,
                        {'ChanId',
                            Mod,
                            Chan
        }   }   }   }   },  #client_state{module = CMod} = ClientState) ->
        case gen_server:call(?MODULE,
            {   subscribe_chan, 
                list_to_atom(Mod),
                list_to_atom(Chan),
                ClientState}) of
            ok ->
                CMod:send(ClientState, pdu(subscribeChanOk, {Mod, Chan}));
            _  ->
                CMod:send(ClientState, pdu(subscribeChanErr,{Mod, Chan}))
        end;

handle_client_msg(
        {message,
            {modIfPDU,
                {fromClient,
                    {unsubscribeChan,
                        {'ChanId',
                            Mod,
                            Chan
        }   }   }   }   },  #client_state{module = CMod} = ClientState) ->
        case gen_server:call(?MODULE,
            {   unsubscribe_chan, 
                list_to_atom(Mod),
                list_to_atom(Chan), 
                ClientState  }) of
            ok ->
                CMod:send(ClientState, pdu(unsubscribeChanOk, {Mod,Chan}));
            _  ->
                CMod:send(ClientState, pdu(unsubscribeChanErr, {Mod,Chan}))
        end;

handle_client_msg(Any, _ClientState) ->
    io:format("-----------~p~n", [Any]).

client_broadcast([], _) ->
    ok;
client_broadcast([#client_state{module = CMod} = Client |OtherClients], Msg) ->
    spawn(fun() -> CMod:send(Client, Msg) end),
    client_broadcast(OtherClients, Msg).
    
% server PDUs
% @private
pdu(authReq, Type) ->
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
                ChanId }}}};

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

pdu(subscribeChanOk, {Module, Chan}) ->
    {modIfPDU, 
        {fromServer, 
            {subscribeChanOk, 
                {'ChanId',
                    Module,
                    Chan }}}};

pdu(subscribeChanErr, {Module, Chan}) ->
    {modIfPDU, 
        {fromServer, 
            {subscribeChanErr, 
                {'ChanId',
                    Module,
                    Chan }}}};

pdu(unsubscribeChanOk, {Module, Chan}) ->
    {modIfPDU, 
        {fromServer, 
            {unsubscribeChanOk, 
                {'ChanId',
                    Module,
                    Chan }}}};

pdu(unsubscribeChanErr, {Module, Chan}) ->
    {modIfPDU, 
        {fromServer, 
            {unsubscribeChanErr, 
                {'ChanId',
                    Module,
                    Chan }}}};


pdu(chanInfo, {Module, Channel, Type}) ->
    {modIfPDU,
        {fromServer,
            {chanInfo,
                {'ChanInfo',
                    Module,
                    Channel,
                    Type }}}}.
% UTILS
chan_exist({Mod, Chan}, IfsModList) ->
    case lists:keyfind(Mod, 2, IfsModList) of
        false   -> false;
        IfsMod  ->
            ChanList = IfsMod#ifs_module.chans,
            case lists:keyfind(Chan, 2, ChanList) of
                false   -> false;
                _       -> true
            end
    end.

a_to_s(AtomList) ->
    lists:foldl(fun(Atom, Acc) ->
        [atom_to_list(Atom) | Acc]
    end, [], AtomList).

client_exist_in_list(_, []) ->
    false;
client_exist_in_list(ClientState, [OtherClient | ClientList]) ->
    case are_the_same_client(ClientState,OtherClient) of
        true    -> 
            true;
        false   ->
            client_exist_in_list(ClientState,ClientList)
    end.

are_the_same_client(
        #client_state{
            socket      = Sock,
            addr        = Addr,
            port        = Port,
            user_name   = Uname,
            pid         = Pid,
            ref         = Ref
        },
        #client_state{
            socket      = Sock,
            addr        = Addr,
            port        = Port,
            user_name   = Uname,
            pid         = Pid,
            ref         = Ref
        }) ->
    true;
are_the_same_client(_,_) ->
    false.

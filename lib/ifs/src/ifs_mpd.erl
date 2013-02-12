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
    new_subscriber/4,
    del_subscriber/3,
    chan_delete/2
]).

-record(state, {
    access_control,
    chan_list
}).
%%-------------------------------------------------------------
%% API
%%-------------------------------------------------------------
% @private
start_link(AccessControlMod) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, AccessControlMod, []).

new_subscriber(Mod, Chan, IfsCallback, ClientState) ->
    gen_server:cast(?MODULE, 
        {new_subscriber, Mod, Chan, IfsCallback, ClientState}).

del_subscriber(Mod, Chan, ClientState) ->
    gen_server:cast(?MODULE, 
        {del_subscriber, Mod, Chan, ClientState}).

chan_delete(Mod,Chan) ->
    gen_server:call(?MODULE, {chan_delete, Mod, Chan}).
%%-------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%-------------------------------------------------------------
init(Args) ->
    io:format("~p     jjjjjjjjjj ~p~n", [?MODULE, Args]),
    {ok, 
        #state{
            access_control  = Args,
            chan_list       = []
        }
    }.

handle_call({chan_delete, Mod, Chan}, _F, S) ->
    io:format("chan_delete ~p~p~n", [Mod, Chan]),
    {noreply, S};

handle_call(_R, _F, S) ->
    io:format("handle_call ~p~p~n", [?MODULE, _R]),
    {noreply, S}.

% CAST
handle_cast({new_subscriber, Mod, Chan, IfsCallback, ClientState}, S) ->
    io:format("new_subscriber ~p :~p ~p ~p~n", [ClientState, IfsCallback, Mod, Chan]),
    {noreply, S};

handle_cast({del_subscriber, Mod, Chan, ClientState}, S) ->
    io:format("del_subscriber ~p : ~p ~p~n", [ClientState, Mod, Chan]),
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

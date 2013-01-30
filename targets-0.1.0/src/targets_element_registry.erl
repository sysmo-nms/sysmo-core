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
-module(targets_element_registry).
-behaviour(gen_server).

-export([
    init/1,
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

-export([
    start_link/0,
    register_name/2,
    unregister_name/1,
    whereis_name/1,
    send/2
]).

%%-------------------------------------------------------------
%% API
%%-------------------------------------------------------------
% @doc start the server.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec register_name(atom(), pid()) -> yes | no.
register_name(Name, Pid) ->
    gen_server:call(?MODULE, {register, Name, Pid}).

-spec unregister_name(atom()) -> ok.
unregister_name(Name) ->
    gen_server:call(?MODULE, {unregister, Name}).

-spec whereis_name(atom()) -> pid() | undefined.
whereis_name(Name) ->
    gen_server:call(?MODULE, {whereis, Name}).

-spec send(atom(), any()) -> pid() | {badarg, {atom(), any()}}.
send(Name, Msg) ->
    gen_server:call(?MODULE, {send, Name, Msg}).

%% GEN_SERVER CALLBACKS
%%-------------------------------------------------------------
init([]) ->
    {ok, []}.

handle_call({register, Name, Pid}, _F, S) ->
    case lists:keymember(Name, 1, S) of
        true ->
            NewS = S,
            Rep = no;
        false ->
            case lists:keymember(Pid, 2, S) of
                true ->
                    NewS = S,
                    Rep = no;
                false ->
                    NewS = [{Name, Pid} | S],
                    Rep = yes
            end
    end,
    {reply, Rep, NewS};

handle_call({unregister, Name}, _F, S) ->
    NewS = lists:keydelete(Name, 1, S),
    {reply, ok, NewS};

handle_call({whereis, Name}, _F, S) ->
    case lists:keyfind(Name, 1, S) of
        false ->
            Rep = undefined;
        {Name, Pid} ->
            Rep = Pid
    end,
    {reply, Rep, S};

handle_call({send, Name, Msg}, _F, S) ->
    case lists:keyfind(Name) of
        false ->
            {stop, {badarg, {Name, Msg}}, {badarg, {Name,Msg}},S};
        {_,Pid} ->
            Pid ! Msg,
            {reply, Pid, S}
    end;

handle_call(_R, _F, S) ->
    {noreply, S}.

% CAST
handle_cast(_R, S) ->
    {noreply, S}.

% OTHER
handle_info(_I, S) ->
    {noreply, S}.

terminate({badarg, {Name, Msg}}, _S) ->
    {badarg, {Name, Msg}};

terminate(_R, _S) ->
    normal.

code_change(_O, S, _E) ->
    {ok, S}.

% This file is part of "Enms" (http://sourceforge.net/projects/enms/)
% Copyright (C) 2012 <Sébastien Serre sserre.bx@gmail.com>
% 
% Enms is a Network Management System aimed to manage and monitor SNMP
% target, monitor network hosts and services, provide a consistent
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
-module(monitor_digraph).
-behaviour(gen_server).
-include("include/monitor.hrl").

% GEN_SERVER
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

% SRV
-export([
    start_link/0
]).

% API
-export([
    init_graph/1
]).

-record(state, {
    graph
}).

-record(label, {
    name,
    ts_set = 0
}).


init_graph(Vertexes) ->
    gen_server:call(?MODULE, {init_graph, Vertexes}).

do_init_graph(G, Vertexes) ->
    mnesia:subscribe({table, probe, detailed}),
    lists:foreach(fun({N,_,L}) ->
        digraph:add_vertex(G, N, #label{name=L})
    end, Vertexes),
    lists:foreach(fun({N,Edges,_}) -> 
        lists:foreach(fun(E) ->
            digraph:add_edge(G,E,N)
        end, Edges)
    end, Vertexes).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%----------------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%----------------------------------------------------------------------------
init([]) ->
    G = digraph:new([acyclic]),
    {ok, #state{graph=G}}.

handle_call({init_graph, Vertexes}, _F, #state{graph=G} = S) ->
    do_init_graph(G, Vertexes),
    {reply, ok, S}.

handle_info({mnesia_table_event, {write, probe, Probe, [], _ActivityId}}, S) ->
    handle_probe_create(S#state.graph, Probe),
    {noreply, S};
handle_info({mnesia_table_event, {write, probe, NewProbe, [OldProbe], _ActivityId}}, S) ->
    handle_probe_update(S#state.graph, NewProbe, OldProbe),
    {noreply, S};
handle_info({mnesia_table_event, {delete, probe, What, _OldRecords, _ActivityId}}, S) ->
    handle_delete(S#state.graph, What),
    {noreply, S};

handle_info(_I, S) -> {noreply, S}.
handle_cast(_R, S) -> {noreply, S}.
terminate(_R, _) -> normal.
code_change(_O, S, _E) -> {ok, S}.


handle_probe_create(G, #probe{name=Name,parents=Edges,status=Label}) ->
    digraph:add_vertex(G, Name, Label),
    lists:foreach(fun(Edge) ->
        digraph:add_edge(G, Name, Edge)
    end, Edges),
    ?LOG(create).

handle_probe_update(G, #probe{name=Name,parents=Edges,status=Label}, _) ->
    digraph:add_vertex(G, Name, Label),

    lists:foreach(fun(Edge) ->
        digraph:del_edge(G, Edge)
    end, digraph:edges(G,Name)),

    lists:foreach(fun(Edge) ->
        digraph:add_edge(G, Name, Edge)
    end, Edges).

handle_delete(G, Key) ->

    digraph:del_vertex(G,Key).

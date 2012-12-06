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
-module(esnmp_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).

-include_lib("../include/qlc.hrl").
-include_lib("../include/esnmp.hrl").
%-include_lib("../../pdu/build/ModEsnmp.hrl").
-include_lib("../../pdu/build/ModEsnmp.hrl").

-define(DEF_AGENT_PORT, 161).
-define(MPD_MOD, ifs_mpd).

-record(esnmp_state, {
    conf_dir
    }).
%%----------------------------------------------------
%% API
%%----------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%----------------------------------------------------
%% GEN_SERVER CALLBACKS
%%----------------------------------------------------
init([]) ->
    % only the first time 
    % ok = esnmp:init_db(),
    Dir         = code:priv_dir(ifsmod_esnmp) ++ "/snmp/mibs/",
    {ok, Env}   = application:get_env(snmp,manager),
    {value, {config,Config}} = lists:keysearch(config, 1, Env),
    {value, {dir, ConfDir}} = lists:keysearch(dir, 1, Config),

    snmpm:load_mib(Dir ++ "RFC1213-MIB"),
    {ok, _} = mnesia:subscribe({table, snmp_trap,  simple}),
    ok      = ifs_server:register_mod({esnmp, modEsnmpPDU}),
    {ok, #esnmp_state{conf_dir = ConfDir}}.


%% CALL
handle_call(R, F, S) ->
    io:format("~p ~p ~p ~p~n", [?MODULE, ?LINE, R, F]),
    {noreply, S}.


%% CAST
handle_cast(R, S) ->
    io:format("~p ~p ~p ~n", [?MODULE, ?LINE, R]),
    {noreply, S}.


%% INFO
handle_info({mnesia_table_event, Data}, S) ->
    handle_event(Data),
    {noreply, S};

handle_info({agent_conf_event, {_Action, _Data, _CommunityPerm}}, S) ->
    % TODO data (ou meme un seul argument [Data, Comm]) doit etre un term() accepté par les encodeurs
    %?MPD_MOD:handle_event({?MODULE, Action, Data, CommunityPerm}),
    {noreply, S};

handle_info(I, S) ->
    io:format("~p ~p ~p ~n", [?MODULE, ?LINE, I]),
    {noreply, S}.


%% OTHER
terminate(R, _S) ->
    io:format("~p ~p ~p ~n", [?MODULE, ?LINE, R]),
    normal.

code_change(_O, S, _E) ->
    io:format("~p ~p ~n", [?MODULE, ?LINE]),
    {ok, S}.


%%----------------------------------------------------
%% PRIVATE
%%----------------------------------------------------
% @private
community_to_read_roles({community, CommunityString}) ->
    Q = qlc:q([E#community_perm.r_roles || E <- mnesia:table(community_perm),
            E#community_perm.community == CommunityString]),
    case mnesia:transaction(fun() -> qlc:e(Q) end) of
        {atomic, []}    -> ["admin"];
        {atomic, [R]}   -> R;
        Other           -> io:format("~p line: ~p erreur: ~p~n", [?MODULE, ?LINE, Other])
    end.

%% @doc EVENT HANDLING
handle_event({write, {snmp_trap, Message, _Pdu, Version, 
                                FromAddr, FromPort, _UserData, Tags, Permissions}, _Id}) ->
    Pdu = {modEsnmpPDU, {fromServer, {trapsTableWrite, [#'TrapsTableRow'{
        timeStamp       = "time",
        version         = lists:flatten(io_lib:format("~p", [Version])),
        fromIp          = lists:flatten(io_lib:format("~p", [FromAddr])),
        fromPort        = lists:flatten(io_lib:format("~p", [FromPort])),
        credentials     = lists:flatten(io_lib:format("~p", [Permissions])),
        tags            = lists:flatten(io_lib:format("~p", [Tags])),
        message         = lists:flatten(io_lib:format("~p", [Message])) }]}}},
    ReadRoles = community_to_read_roles(Permissions),
    ?MPD_MOD:handle_event(esnmp, Pdu, ReadRoles),
    ok;

handle_event(Other) ->
    io:format("received other ~p ~p ~p~n", [?MODULE, ?LINE, Other]).

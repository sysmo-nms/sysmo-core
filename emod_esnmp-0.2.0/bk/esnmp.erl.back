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
-module(esnmp).
-behaviour(beha_ifs_module).

% beha_ifs_module exports
-export([           
    handle_msg/2,
    init_client/1]).

-export([
    register_v2agent/4,
    gen_id/0,
    get_conf_dir/0,
    % config
    get_manager_conf/0,
    get_users_conf/0,
    get_agents_conf/0,
    get_usm_conf/0,
    init_db/0,
    module_roles/4]).

-include_lib("../include/esnmp.hrl").
-include_lib("../../ifs-0.1.0/include/client_state.hrl").
-define(DEF_AGENT_PORT, 161).

%% API
% module_perm ajoute un role a r ou rw sur le module. ifs_server ne soumet au client
% que les modules auxquels l'utilisateur a un role r ou rw.
module_roles(add, Read, ReadWrite, RoleName) ->
    DbRec = #esnmp_perm{
        role        = RoleName,
        read        = Read,
        read_write  = ReadWrite},
    {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(esnmp_perm, DbRec, write) end).

% get Configuration informations
get_manager_conf() ->
    snmpm_conf:read_manager_config(get_conf_dir()).

get_users_conf() ->
    snmpm_conf:read_users_config(get_conf_dir()).

get_agents_conf() ->
    snmpm_conf:read_agents_config(get_conf_dir()).

get_usm_conf() ->
    snmpm_conf:read_usm_config(get_conf_dir()).


%% MESSAGE FROM CLIENTS
init_client(#client_state{module = Module} = ClientState) ->
    % Send all agents configuration:
    {ok, AConf} = get_agents_conf(),
    AConfAsn = lists:map(fun({_, T, C, [I1,I2,I3,I4], P, E, To, Ms, V, Vs, Sm, Auth}) ->
        {'AgentConf', 
            T, C, 
            {'IpAddr', I1,I2,I3,I4},
            P, E, To, Ms,
            erlang:atom_to_list(V),
            erlang:atom_to_list(Vs),
            Sm,
            erlang:atom_to_list(Auth)
        }
    end, AConf),
    Module:send(ClientState, {modEsnmpPDU, {fromServer, {agentsConf, AConfAsn}}}).

handle_msg(Msg, ClientState) ->
    io:format("~p ~p~n", [Msg, ClientState]).

register_v2agent(UserMod, Addr, Port, Community) ->
    TargetName  = gen_id(),
    Conf        =  [{engine_id, "initial"},
                    {community, Community},
                    {port,      Port},
                    {address,   Addr}],
    case snmpm:register_agent(UserMod, TargetName, Conf) of
        {error, {already_registered,_}} ->
            register_v2agent(UserMod, Addr, Port, Community);
        ok ->
            % write the new agent in the config file agents.conf
            AgentEntry  = snmpm_conf:agents_entry(generic_user, TargetName, Community, 
                            erlang:tuple_to_list(Addr), Port, "engine_id", 1500, 484, v2, v2c,
                                "initial", noAuthNoPriv),
            snmpm_conf:append_agents_config(get_conf_dir(), [AgentEntry]),
            esnmp_server ! {agent_conf_event, {write, AgentEntry, Community}},
            ok;
        Any ->
            Any
    end.

% @private
gen_id() ->
    AllowedChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
    Id = lists:foldl(fun(_, Acc) ->
        [lists:nth(random:uniform(length(AllowedChars)), AllowedChars)]
        ++ Acc
    end, [], lists:seq(1, 10)),
    "targetName-" ++ Id.

get_conf_dir() ->
    {ok,             A}     = application:get_env(snmp, manager),
    {value, {config, B}}    = lists:keysearch(config, 1, A),
    {value, {dir,    C}}    = lists:keysearch(dir, 1, B),
    C.

init_db() ->
    IsRunning       = mnesia:system_info(is_running),
    DirUsed         = mnesia:system_info(use_dir),
    if IsRunning    == yes  -> mnesia:stop(); true -> ok end,
    if DirUsed      == true -> mnesia:delete_schema([node()]); true -> ok end,
    ok  = mnesia:create_schema([node()]),
    ok  = mnesia:start(),
    {atomic, ok} = mnesia:create_table(snmp_trap, [{attributes, record_info(fields, snmp_trap)},
            {record_name, snmp_trap}, {disc_only_copies, [node()]}]),
    {atomic, ok} = mnesia:create_table(community_perm, [{attributes, record_info(fields, community_perm)},
            {record_name, community_perm}, {disc_copies, [node()]}]),
    {atomic, ok} = mnesia:create_table(esnmp_perm, [{attributes, record_info(fields, esnmp_perm)},
            {record_name, esnmp_perm}, {disc_copies, [node()]}]),
    DefV2AgentPerm = #community_perm{
        community = "unknown_agent",
        r_roles = ["admin", "wheel"],
        rw_roles = ["admin"]},
    {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(community_perm, DefV2AgentPerm, write) end),
    ok.

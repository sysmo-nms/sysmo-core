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
-module(esnmp_init).
-compile(export_all).

-include_lib("../include/esnmp.hrl").
-include_lib("../include/qlc.hrl").

init_mnesia() ->
	IsRunning = mnesia:system_info(is_running),
	DirUsed = mnesia:system_info(use_dir),
	if IsRunning == yes -> mnesia:stop(); true -> ok end,
	if DirUsed == true -> mnesia:delete_schema([node()]); true -> ok end,
	ok = mnesia:create_schema([node()]),
	ok = mnesia:start(),
	ok = create_table().

create_table() ->
	{atomic, ok} = mnesia:create_table(snmp_agents,
		[{attributes, record_info(fields, snmp_agent)},
			{record_name, snmp_agent}, {disc_copies, [node()]}]),
	{atomic, ok} = mnesia:create_table(snmp_traps,
		[{attributes, record_info(fields, snmp_trap)},
			{record_name, snmp_trap}, {disc_copies, [node()]}]),
	{atomic, ok} = mnesia:create_table(snmp_usm_users,
		[{attributes, record_info(fields, snmp_usm_user)},
			{record_name, snmp_usm_user}, {disc_copies, [node()]}]),
	ok.

system_boot() ->
	QAgents = qlc:q([X || X <- mnesia:table(snmp_agents)]),
	QUsmUsers = qlc:q([X || X <- mnesia:table(snmp_usm_users)]),
	{atomic, Agents} = mnesia:transaction(fun() ->qlc:e(QAgents) end),
	{atomic, UsmUsers} = mnesia:transaction(fun() ->qlc:e(QUsmUsers) end),
	lists:foreach(fun(X) ->
		{EngineId, UsmUser} = X#snmp_usm_user.id,
		snmpm:register_usm_user(EngineId, UsmUser, [{auth, usmNoAuthProtocol}, {priv, usmNoPrivProtocol}])
	end, UsmUsers),
	lists:foreach(fun(X) ->
		case X#snmp_agent.version of
			v2 ->
				snmpm:register_agent(X#snmp_agent.snmpm_user, X#snmp_agent.id,
					[
						{engine_id, X#snmp_agent.engine_id},
						{port, X#snmp_agent.port},
						{address, X#snmp_agent.addr},
						{community, X#snmp_agent.community}
					]);
			v3 ->
				_Val = snmpm:register_agent(X#snmp_agent.snmpm_user, X#snmp_agent.id,
					[
						{engine_id, X#snmp_agent.engine_id},
						{port, X#snmp_agent.port},
						{address, X#snmp_agent.addr},
						{version, v3},
						{sec_model, usm},
						{sec_level, X#snmp_agent.sec_level},
						{sec_name, X#snmp_agent.sec_name}
					]);
			Other ->
				io:format("unknown other: ~p~n", [Other])
		end
	end, Agents).

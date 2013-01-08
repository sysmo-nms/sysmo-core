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
-module(esnmp_targets).
-compile(export_all).
-include("../include/esnmp.hrl").




%% SET TARGET CONF
% @doc Add or modify a v2 agent entry
set_v2_agent(_AgentConf) ->
    ok.

% @doc Add or modify a v3 agent entry
set_v3_agent(_AgentConf) ->
    ok.

% @doc delete an agent entry
del_agent(_AgentId) ->
    ok.




%% PROBES
% @doc Add or modify a probe configuration
set_probe(_ProbeConf, _AgentId) ->
    ok.

% @doc delete a probe configuration
del_probe(_ProbeId) ->
    ok.


%% COMMUNITY TO ROLES
% @doc set or modify community to role table
set_ctr(_Community, _Role) ->
    ok.

% @doc del community to role entry
del_ctr(_Community, _Role) ->
    ok.

% @doc get community to role table
get_ctr(_Community, _Role) ->
    ok.

% @doc get roles from a community and the reverse
get_ctr({community, _Community}) ->
    ok;

get_ctr({role, _Role}) ->
    ok.


%% HELPERS
% @private
% @doc get agent informations
get_agent(AgentId) ->
    case a_info(AgentId, engine_id) of
        {error, not_found} ->
            {error, not_found};
        EngineId ->
            AgentInfo = #snmp_agent{
                engine_id   = EngineId,
                address     = a_info(AgentId, address),
                port        = a_info(AgentId, port),
                tdomain     = a_info(AgentId, tdomain),
                community   = a_info(AgentId, community),
                timeout     = a_info(AgentId, timeout),
                max_message_size = a_info(AgentId, max_message_size),
                version     = a_info(AgentId, version),
                sec_model   = a_info(AgentId, sec_model),
                sec_name    = a_info(AgentId, sec_name),
                sec_level   = a_info(AgentId, sec_level)
            },
            {ok, AgentInfo}
    end.

% @private
% @doc get agent info
a_info(AgentId, Val) ->
    case snmpm:agent_info(AgentId, Val) of
        {ok, Ret} -> Ret;
        Error     -> Error
    end.

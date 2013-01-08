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




% @doc Add or modify a v2 agent entry
set_v2_agent(AgentConf) ->
    ok.

% @doc Add or modify a v3 agent entry
set_v3_agent(AgentConf) ->
    ok.

% @doc delete an agent entry
del_agent(AgentId) ->
    ok.




% @doc Add or modify a probe configuration
set_probe(ProbeConf, AgentId) ->
    ok.

% @doc delete a probe configuration
del_probe(ProbeId) ->
    ok.




%% HELPERS
% @private
% @doc get agent information
get_agent(Id) ->
    case a_info(Id, engine_id) of
        {error, not_found} ->
            {error, not_found};
        EngineId ->
            #snmp_agent{
                engine_id   = EngineId,
                address     = a_info(Id, address),
                port        = a_info(Id, port),
                tdomain     = a_info(Id, tdomain),
                community   = a_info(Id, community),
                timeout     = a_info(Id, timeout),
                max_message_size = a_info(Id, max_message_size),
                version     = a_info(Id, version),
                sec_model   = a_info(Id, sec_model),
                sec_name    = a_info(Id, sec_name),
                sec_level   = a_info(Id, sec_level)
            }
    end.

a_info(Id, Val) ->
    case snmpm:agent_info(Id, Val) of
        {ok, Ret} -> Ret;
        Error     -> Error
    end.

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
-record(snmp_trap, {
				snmp_message,   % erlang term extracted from the pdu
                snmp_pdu,       % raw pdu
				version,
				from_addr,
				from_port,
				user_data,
                tags,
                permissions}).

-record(community_perm, {
    community,
    r_roles,
    rw_roles}).

-record(esnmp_perm, {
    role, 
    read,
    read_write}).

-record(snmp_agent, {
				id,
				version,
				snmpm_user,
				sec_name,
				sec_level,
				addr,
				port,
				community,
				engine_id,
				target_name,
				added_date,
				updade_date,
				tags,
                extra,
				user_data,
                permissions
	}).

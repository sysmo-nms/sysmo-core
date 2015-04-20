% This file is part of "Enms" (http://sourceforge.net/projects/enms/)
% Copyright (C) 2012 <Sébastien Serre sserre.bx@gmail.com>
% 
% Enms is a Network Management System aimed to manage and monitor SNMP
% monitors, monitor network hosts and services, provide a consistent
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
{application, monitor,
    [
        {description, "Data store of monitors configuration"},
        {vsn, "0.1.0"},
        {modules, [
            monitor,
            monitor_app,
            monitor_channel,
            monitor_commander,
            monitor_data_master,
            monitor_events,
            monitor_jobs,
            monitor_pdu,
            monitor_sup,
            monitor_utils,
            probe_simple_nchecks,
            probe_simple_nchecks_sup,
            snmp_ifPerf,
            snmp_ifPerf_sup
        ]},
        {registered, [
                monitor_commander,
                monitor_data_master,
                monitor_events,
                monitor_sup,
                snmp_ifPerf_sup,
                probe_nchecks_sup
            ]},
        {applications, 
            [kernel, stdlib, xmerl, mnesia, supercast, equartz, errd4j, snmpman]
        },
        {start_phases, []},
        {mod, {monitor_app, []}}
    ]
}.


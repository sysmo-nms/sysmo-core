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
            bmonitor_inspector_fire_alert,
            bmonitor_inspector_property_set,
            bmonitor_inspector_status_set,
            bmonitor_logger_rrd,
            bmonitor_logger_rrd2,
            bmonitor_probe_nchecks,
            bmonitor_probe_snmp,
            monitor,
            monitor_app,
            monitor_channel,
            monitor_commander,
            monitor_data_master,
            monitor_events,
            monitor_exec,
            monitor_inspector,
            monitor_jobs,
            monitor_logger,
            monitor_pdu,
            monitor_probe,
            monitor_probe_sup,
            monitor_sup,
            monitor_utils,

            probe_nchecks,
            probe_snmp_get,
            probe_snmp_walk
        ]},
        {registered, [
                monitor_sup,
                'target-MasterChan',
                monitor_commander,
                monitor_probe_sup
            ]},
        {applications, 
            [kernel, stdlib, mnesia, supercast, errd, snmpman, ini]
        },
        {start_phases, []},
        {mod, {monitor_app, []}}
    ]
}.


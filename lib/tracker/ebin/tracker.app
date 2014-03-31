% This file is part of "Enms" (http://sourceforge.net/projects/enms/)
% Copyright (C) 2012 <Sébastien Serre sserre.bx@gmail.com>
% 
% Enms is a Network Management System aimed to manage and monitor SNMP
% trackers, monitor network hosts and services, provide a consistent
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
{application, tracker,
    [
        {description, "Data store of trackers configuration"},
        {vsn, "0.1.0"},
        {modules, [
                tracker_app,
                tracker_api,
                tracker_master_channel,
                tracker_misc,
                tracker_probe_sup,
                tracker_probe,
                tracker_target_channel,
                tracker_target_channel_sup,
                tracker_sup,
                btracker_inspector_property_set,
                btracker_inspector_status_set,
                btracker_inspector_parent,
                btracker_logger_rrd,
                btracker_logger_text,
                btracker_logger_events,
                btracker_probe_nagios,
                btracker_probe_snmp,
                btracker_probe_standard_snmp
            ]},
        {registered, [
                tracker_sup,
                'target-MasterChan',
                tracker_misc,
                tracker_probe_sup,
                tracker_target_channel_sup
            ]},
        {applications, 
            [kernel, stdlib, mnesia, supercast, tlogger_rrd,
                tlogger_text, snmp, tracker_events]
        },
        {start_phases,
            [
                {create_targets, []}
            ]
        },
        {mod, {tracker_app, []}}
    ]
}.

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
% @private
-module(monitor).
-include("include/monitor.hrl").
-include("include/monitor_snmp.hrl").

-export([
    add_target/1,
    del_target/0,
    update_target/0,
    add_probe/2,
    del_probe/0,
    update_probe/0
]).

add_target(Name) ->
    Target = #target{
        id = Name,
        properties = [
            {"ip",          "192.168.0.5"},
            {"ipVersion",   "v4"},
            {"staticName",  lists:concat(["testouille",Name])},
            {"dnsName",     "undefined"},
            {"sysName",     "undefined"}
        ]
    },
    monitor_master:create_target(Target).



del_target() -> ok.
update_target() -> ok.

add_probe(Target, Name) ->
    Probe = #probe{
        name = Name,

        monitor_probe_mod = bmonitor_probe_nchecks,
        monitor_probe_conf = #nchecks_probe_conf{
            function    = icmp,
            args        = []
        },
        inspectors  = [
            #inspector{
                module   = bmonitor_inspector_status_set,
                conf     = []
            }
        ]
    },
    monitor_master:create_probe(Target, Probe).


del_probe() -> ok.
update_probe() -> ok.

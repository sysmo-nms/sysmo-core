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
-module(monitor_templates).
-include("include/monitor.hrl").
-include("../snmp_manager/include/snmp_manager.hrl").

-export([
    generate_icmpProbe/2,
    generate_sysLocNameProbe/2,
    generate_ifPerfProbe/2
]).

-define(PERF_IFTYPES, [
    6,          % ethernetCsmacd
    209         % bridge
]).

generate_icmpProbe(ProbeId, Target) ->
    {ok, 
        #probe{
            id          = 0,
            name        = ProbeId,
            description = "ICMP Echo request",
            info        = "
                Trigger a single echo request every 30 seconds
            ",
            permissions = Target#target.global_perm,
            monitor_probe_mod   = bmonitor_probe_nagios,
            monitor_probe_conf  = #nagios_plugin_conf{
                 executable = "/opt/nagios-plugins-1.4.16/libexec/check_icmp",
                 args       = [{"-H", Target#target.ip}, {"-t", "5"}],
                 eval_perfs = false
            },
            status      = 'UNKNOWN',
            timeout     = 5,
            %step        = 30,
            step        = 5,
            inspectors  = [
                #inspector{
                    module  = bmonitor_inspector_status_set,
                    conf    = []
                },
                #inspector{
                    module  = bmonitor_inspector_property_set,
                    conf    = ["status"]
                }
            ],
            loggers     = [
                #logger{
                    module  = bmonitor_logger_text,
                    conf    = []
                }
            ],
            parents     = [],
            properties  = [],
            active      = true
        }
    }.

generate_sysLocNameProbe(ProbeId, Target) ->
    Community = proplists:get_value(snmp_ro, Target#target.properties),
    {ok, 
        #probe{
            id          = 1,
            name        = ProbeId,
            description = "SNMP: sysInfo set",
            info        = "
                Set the target name and location properties depending on the
                MIB2 sysName and sysLocation OIDs every 5 minutes
            ",
            permissions = Target#target.global_perm,
            monitor_probe_mod   = bmonitor_probe_snmp,
            monitor_probe_conf  = #snmp_conf{
                port        = 161,
                version     = v2,
                community   = Community,
                oids        = [
                    {"sysName",  [1,3,6,1,2,1,1,5,0]},
                    {"location", [1,3,6,1,2,1,1,6,0]}
                ]
            },
            status      = 'UNKNOWN',
            timeout     = 5,
            %step        = 300,
            step        = 5,
            inspectors  = [
                #inspector{
                    module  = bmonitor_inspector_status_set,
                    conf    = []
                },
                #inspector{
                    module  = bmonitor_inspector_property_set,
                    conf    = ["status", "sysName", "location"]
                }
            ],
            loggers     = [
                #logger{
                    module  = bmonitor_logger_text,
                    conf    = []
                }
            ],
            parents     = [],
            properties  = [],
            active      = true
        }
    }.

generate_ifPerfProbe(ProbeId, Target) ->
    Community = proplists:get_value(snmp_ro, Target#target.properties),
    Ip        = proplists:get_value(ip,      Target#target.properties),
    % TODO handle SNMP v3
    TmpArgs = [
        {engine_id, "none"},
        {address,   Ip},
        {port,      161},
        {version,   v2},
        {community, Community}
    ],
    TmpAgent    = snmp_manager:register_temporary_agent(TmpArgs),
    Ifs         = snmp_manager:get_mib2_interfaces(TmpAgent),
    Ifs2        = filter_if_for_perfs(Ifs),
    Ifs3        = rename_if_needed(Ifs2),
    {QueryOids, RrdConf}   = generate_conf(Ifs3),
    ?LOG({QueryOids, RrdConf}),
    {ok,
        #probe{
            id          = 2,
            name        = ProbeId,
            description = "SNMP: Interfaces performances",
            info        = "
            Query the element MIB-2 interface tree every 2 minutes and store 
            the results in a rrd database.
            ",
            permissions = Target#target.global_perm,
            monitor_probe_mod   = bmonitor_probe_snmp,
            monitor_probe_conf  = #snmp_conf{
                port        = 161,
                version     = v2,
                community   = Community,
                oids        = [
                    {"sis0in",     [1,3,6,1,2,1,2,2,1,10,1,0]},
                    {"sis0out",    [1,3,6,1,2,1,2,2,1,16,1,0]},
                    {"sis1in",     [1,3,6,1,2,1,2,2,1,10,2,0]},
                    {"sis1out",    [1,3,6,1,2,1,2,2,1,16,2,0]}
                ]
            },
            status      = 'UNKNOWN',
            timeout     = 5,
            %step        = 120,
            step        = 5,
            inspectors  = [
                #inspector{
                    module  = bmonitor_inspector_status_set,
                    conf    = []
                },
                #inspector{
                    module  = bmonitor_inspector_property_set,
                    conf    = ["status"]
                }
            ],
            loggers     = [
                #logger{
                    module  = bmonitor_logger_text,
                    conf    = []
                },
                #logger{
                    module  = bmonitor_logger_rrd,
                    conf    = [
                        #rrd_config{
                            file    = atom_to_list(ProbeId) ++ "1.rrd",
                            create  =  "create <FILE> --step 5 DS:sis0in:COUNTER:10:U:U DS:sis0out:COUNTER:10:U:U RRA:AVERAGE:0.5:1:600 RRA:AVERAGE:0.5:6:700 RRA:AVERAGE:0.5:24:775 RRA:AVERAGE:0.5:288:797",
                            update  = "update <FILE> --template sis0in:sis0out N:<SIS0-IN>:<SIS0-OUT>",
                            graphs  = [
                                "DEF:s0in=<FILE>:sis0in:AVERAGE DEF:s0out=<FILE>:sis0out:AVERAGE LINE1:s0in#3465A4 LINE2:s0out#CC0000"
                            ],
                            binds   = [
                                 {"sis0in",  "<SIS0-IN>"},
                                 {"sis0out", "<SIS0-OUT>"}
                            ],
                            update_regexps  = none,
                            file_path       = none
                        },
                        #rrd_config{
                            file    = atom_to_list(ProbeId) ++ "2.rrd",
                            create  =  "create <FILE> --step 5 DS:sis1in:COUNTER:10:U:U DS:sis1out:COUNTER:10:U:U RRA:AVERAGE:0.5:1:600 RRA:AVERAGE:0.5:6:700 RRA:AVERAGE:0.5:24:775 RRA:AVERAGE:0.5:288:797",
                            update  = "update <FILE> --template sis1in:sis1out N:<SIS1-IN>:<SIS1-OUT>",
                            graphs  = [
                                "DEF:s0in=<FILE>:sis1in:AVERAGE DEF:s0out=<FILE>:sis1out:AVERAGE LINE1:s0in#3465A4 LINE2:s0out#CC0000"
                            ],
                            binds   = [
                                 {"sis1in",  "<SIS1-IN>"},
                                 {"sis1out", "<SIS1-OUT>"}
                            ],
                            update_regexps  = none,
                            file_path       = none
                        }
                    ]
                }
            ],
            parents     = [],
            properties  = [],
            active      = true
        }
    }.

% @private
% Only if of type ?PERF_IFTYPES
filter_if_for_perfs(Ifs) ->
    filter_if_for_perfs(Ifs, []).
filter_if_for_perfs([], Acc) -> Acc;
filter_if_for_perfs([If|Other], Acc) ->
    case lists:member(If#mib2_ifEntry.ifType, ?PERF_IFTYPES) of
        true ->
            filter_if_for_perfs(Other, [If|Acc]);
        false ->
            filter_if_for_perfs(Other, Acc)
    end.

% some interfaces might have the same descr, add "_" to the end.
rename_if_needed(Ifs) -> 
    Names = [Name || #mib2_ifEntry{ifDescr = Name} <- Ifs],
    rename_if_needed(Ifs, lists:reverse(Names)).
rename_if_needed(Ifs, []) -> Ifs;
rename_if_needed(Ifs, [Name|Names]) ->
    case lists:member(Name, Names) of
        false   -> rename_if_needed(Ifs, Names);
        true    -> 
            {value, If, Ifs2} = lists:keytake(Name, 3, Ifs),
            NewName           = Name ++ "_",
            If2               = If#mib2_ifEntry{ifDescr = NewName},
            rename_if_needed([If2|Ifs2], [NewName | Names])
    end.

% generate snmp_conf oids and rrd_config
% [{"sis0in",     [1,3,6,1,2,1,2,2,1,10,1,0]},
% {"sis0out",    [1,3,6,1,2,1,2,2,1,16,1,0]},
% {"sis1in",     [1,3,6,1,2,1,2,2,1,10,2,0]},
% {"sis1out",    [1,3,6,1,2,1,2,2,1,16,2,0]}]
%
% {rrd_config,
% "secondTestHost-1_rrd1",
% "create <FILE> --step 5 DS:sis0in:COUNTER:10:U:U DS:sis0out:COUNTER:10:U:U RRA:AVERAGE:0.5:1:600 RRA:AVERAGE:0.5:6:700 RRA:AVERAGE:0.5:24:775 RRA:AVERAGE:0.5:288:797",
% "update <FILE> --template sis0in:sis0out N:<SIS0-IN>:<SIS0-OUT>",
% [
% "DEF:s0in=<FILE>:sis0in:AVERAGE DEF:s0out=<FILE>:sis0out:AVERAGE LINE1:s0in#3465A4 LINE2:s0out#CC0000"
% ],
% [
% {"sis0in",  "<SIS0-IN>"},
% {"sis0out", "<SIS0-OUT>"}
% ],
generate_conf(Ifs) ->
    generate_conf(Ifs, {[], []}).
generate_conf([], {OidsAcc, RrdsAcc}) -> 
    {lists:flatten(OidsAcc), lists:flatten(RrdsAcc)};
generate_conf([If|Ifs], {OidsAcc, RrdsAcc}) ->
    Index   = If#mib2_ifEntry.ifIndex,
    Descr   = If#mib2_ifEntry.ifDescr,

    % generate if in out octets
    IfOctetsIn    = Descr ++ "_ifInOctets",
    IfOctetsOut   = Descr ++ "_ifOutOctets",
    OidOctetsIn   = [1,3,6,1,2,1,2,2,1,10,Index,0],
    OidOctetsOut  = [1,3,6,1,2,1,2,2,1,16,Index,0],
    %RrdConf0    = #rrd_config

    % generate if in out packets u/nu and errors
    IfInUcastPkts    = Descr ++ "_ifInUcastPkts",
    IfInNUcastPkts   = Descr ++ "_ifInNUcastPkts",
    IfInErrors       = Descr ++ "_ifInErrors",
    IfOutUcastPkts   = Descr ++ "_ifOutUcastPkts",
    IfOutNUcastPkts  = Descr ++ "_ifOutNUcastPkts",
    IfOutErrors      = Descr ++ "_ifOutErrors",
    
    OidInUcastPkts   = [1,3,6,1,2,1,2,2,1,11,Index,0],
    OidInNUcastPkts  = [1,3,6,1,2,1,2,2,1,12,Index,0],
    OidInErrors      = [1,3,6,1,2,1,2,2,1,14,Index,0],
    OidOutUcastPkts  = [1,3,6,1,2,1,2,2,1,17,Index,0],
    OidOutNUcastPkts = [1,3,6,1,2,1,2,2,1,18,Index,0],
    OidOutErrors     = [1,3,6,1,2,1,2,2,1,20,Index,0],

    % query oids
    Oids = [
        {IfOctetsIn,        OidOctetsIn},
        {IfOctetsOut,       OidOctetsOut},
        {IfInUcastPkts,     OidInUcastPkts},
        {IfInNUcastPkts,    OidInNUcastPkts},
        {IfInErrors,        OidInErrors},
        {IfOutUcastPkts,    OidOutUcastPkts},
        {IfOutNUcastPkts,   OidOutNUcastPkts},
        {IfOutErrors,       OidOutErrors}
    ],

    % TODO rrd_config
    generate_conf(Ifs, {[Oids|OidsAcc], [none|RrdsAcc]}).

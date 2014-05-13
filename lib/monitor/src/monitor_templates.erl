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

-define(RRD_ifOctets_CREATE,
"create <FILE> --step 5 DS:octetsIn:COUNTER:10:U:U DS:octetsOut:COUNTER:10:U:U DS:ucastPkIn:COUNTER:10:U:U DS:nucastPkIn:COUNTER:10:U:U DS:errorsIn:COUNTER:10:U:U DS:ucastPkOut:COUNTER:10:U:U RRA:AVERAGE:0.5:1:600 RRA:AVERAGE:0.5:6:700 RRA:AVERAGE:0.5:24:775 RRA:AVERAGE:0.5:288:797"
).
-define(RRD_ifOctets_UPDATE,
"update <FILE> --template octetsIn:octetsOut:ucastPkIn:nucastPkIn:errorsIn:ucastPkOut N:<OCTETS-IN>:<OCTETS-OUT>:<UCAST-IN>:<NUCAST-IN>:<ERRORS-IN>:<UCAST-OUT>"
).
-define(RRD_ifOctets_GRAPH,
    [
"DEF:oIn=<FILE>:octetsIn:AVERAGE DEF:oOut=<FILE>:octetsOut:AVERAGE LINE1:oIn#3465A4 LINE2:oOut#CC0000"
    ]
).

-define(RRD_ifPkts_CREATE,
"create <FILE> --step 5 DS:<INUPKTS>:COUNTER:10:U:U DS:<OUTUPKTS>:COUNTER:10:U:U DS:<INNUPKTS>:COUNTER:10:U:U DS:<OUTNUPKTS>:COUNTER:10:U:U DS:<INERR>:COUNTER:10:U:U DS:<OUTERR>:COUNTER:10:U:U RRA:AVERAGE:0.5:1:600 RRA:AVERAGE:0.5:6:700 RRA:AVERAGE:0.5:24:775 RRA:AVERAGE:0.5:288:797"
).
-define(RRD_ifPkts_UPDATE,
"update <FILE> --template <INUPKTS>:<OUTUPKTS>:<INNUPKTS>:<OUTNUPKTS>:<INERR>:<OUTERR> N:<UIN>:<UOUT>:<NUIN>:<NUOUT>:<ERRIN>:<ERROUT>"
).
-define(RRD_ifPkts_GRAPH,
    [
"DEF:unicastIn=<FILE>:<INUPKTS>:AVERAGE DEF:unicastOut=<FILE>:<OUTUPKTS>:AVERAGE DEF:nunicastIn=<FILE>:<INNUPKTS>:AVERAGE DEF:nunicastOut=<FILE>:<OUTNUPKTS> DEF:errIn:<FILE>:<INERR> DEF:errOut:<FILE>:<OUTERR> LINE1:unicastIn#ff0000 LINE2:unicastOut#00ff00 LINE3:nunicastIn#0000ff LINE4:nunicastOut#f0f000 LINE5:errIn#ffff00 LINE6:errOut#00ffff"
    ]
).

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
                ],
                method      = get
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
    %?LOG({QueryOids, RrdConf}),
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
                oids        = QueryOids,
                method      = {walk, [
                    [1,3,6,1,2,1,2,2,1,10],
                    [1,3,6,1,2,1,2,2,1,11],
                    [1,3,6,1,2,1,2,2,1,12],
                    [1,3,6,1,2,1,2,2,1,14],
                    [1,3,6,1,2,1,2,2,1,16],
                    [1,3,6,1,2,1,2,2,1,17]
                ]}
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
                    conf    = RrdConf
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

generate_conf(Ifs) ->
    generate_conf(Ifs, {[], []}).
generate_conf([], {OidsAcc, RrdsAcc}) -> 
    {lists:flatten(OidsAcc), lists:flatten(RrdsAcc)};
generate_conf([If|Ifs], {OidsAcc, RrdsAcc}) ->
    Index   = If#mib2_ifEntry.ifIndex,
    Descr   = If#mib2_ifEntry.ifDescr,

    % generate if in out octets
    %RrdConf0    = #rrd_config

    % generate if in out packets u/nu and errors
    IfOctetsIn       = Descr ++ "_ifInOctets",
    IfOctetsOut      = Descr ++ "_ifOutOctets",
    IfInUcastPkts    = Descr ++ "_ifInUcastPkts",
    IfInNUcastPkts   = Descr ++ "_ifInNUcastPkts",
    IfInErrors       = Descr ++ "_ifInErrors",
    IfOutUcastPkts   = Descr ++ "_ifOutUcastPkts",
    %IfOutNUcastPkts  = Descr ++ "_ifOutNUcastPkts",
%     IfOutErrors      = Descr ++ "_ifOutErrors",
%     
    OidOctetsIn      = [1,3,6,1,2,1,2,2,1,10,Index,0],
    OidOctetsOut     = [1,3,6,1,2,1,2,2,1,16,Index,0],
    OidInUcastPkts   = [1,3,6,1,2,1,2,2,1,11,Index,0],
    OidInNUcastPkts  = [1,3,6,1,2,1,2,2,1,12,Index,0],
    OidInErrors      = [1,3,6,1,2,1,2,2,1,14,Index,0],
    OidOutUcastPkts  = [1,3,6,1,2,1,2,2,1,17,Index,0],
    %OidOutNUcastPkts = [1,3,6,1,2,1,2,2,1,18,Index,0],
%     OidOutErrors     = [1,3,6,1,2,1,2,2,1,20,Index,0],

    % query oids
    Oids = [
        {IfOctetsIn,        OidOctetsIn},
        {IfOctetsOut,       OidOctetsOut},
        {IfInUcastPkts,     OidInUcastPkts},
        {IfInNUcastPkts,    OidInNUcastPkts},
        {IfInErrors,        OidInErrors},
        {IfOutUcastPkts,    OidOutUcastPkts}
        %{IfOutNUcastPkts,   OidOutNUcastPkts}
        %{IfOutErrors,       OidOutErrors}
    ],

    OctetsInOutRrdConf = #rrd_config{
        file    = Descr,
        create  = ?RRD_ifOctets_CREATE,
        update  = ?RRD_ifOctets_UPDATE,
        graphs  = ?RRD_ifOctets_GRAPH,
        binds   = [
            {IfOctetsIn,        "<OCTETS-IN>"},
            {IfOctetsOut,       "<OCTETS-OUT>"},
            {IfInUcastPkts,     "<UCAST-IN>"},
            {IfInNUcastPkts,    "<NUCAST-IN>"},
            {IfInErrors,        "<ERRORS-IN>"},
            {IfOutUcastPkts,    "<UCAST-OUT>"}
            %{IfOutNUcastPkts,   "<NUCAST-OUT>"}
            %{IfOutErrors,       "<ERRORS-OUT>"}
        ],
        update_regexps  = none,
        file_path       = none
    },

    generate_conf(Ifs, {[Oids|OidsAcc], [OctetsInOutRrdConf|RrdsAcc]}).

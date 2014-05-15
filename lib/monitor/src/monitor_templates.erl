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

% four RRAs from 100 hours to 10 years
% RRA: (1*300)  * 1200 = 360000    seconds = 6000    minutes = 100   hours
% RRA: (12*300) * 2400 = 8640000   seconds = 14400   minutes = 2400  hours = 100 days

% RRA: (50*300) * 6400 = 96000000  seconds = 1600000 minutes = 26666 hours = 1111 days = 3 years
% RRA: (118*300)* 8900 = 315060000 seconds = 5251000 minutes = 87516 hours = 3646 days = 10 years
-define(RRD_ifOctets_CREATE,
"create <FILE> --step 300 DS:octetsIn:COUNTER:650:U:U DS:octetsOut:COUNTER:650:U:U DS:ucastPkIn:COUNTER:650:U:U DS:nucastPkIn:COUNTER:650:U:U DS:errorsIn:COUNTER:650:U:U DS:ucastPkOut:COUNTER:650:U:U DS:nucastPkOut:COUNTER:650:U:U DS:errorsOut:COUNTER:650:U:U RRA:AVERAGE:0.5:1:1200 RRA:AVERAGE:0.5:12:2400"
).
-define(RRD_ifOctets_UPDATE,
"update <FILE> --template octetsIn:octetsOut:ucastPkIn:nucastPkIn:errorsIn:ucastPkOut:nucastPkOut:errorsOut N:<OCTETS-IN>:<OCTETS-OUT>:<UCAST-IN>:<NUCAST-IN>:<ERRORS-IN>:<UCAST-OUT>:<NUCAST-OUT>:<ERRORS-OUT>"
).
-define(RRD_ifOctets_GRAPH,
    [
"DEF:oIn=<FILE>:octetsIn:AVERAGE DEF:oOut=<FILE>:octetsOut:AVERAGE LINE1:oIn#3465A4 LINE2:oOut#CC0000",
"DEF:errIn=<FILE>:errorsIn:AVERAGE DEF:errOut=<FILE>:errorsOut:AVERAGE LINE1:errIn#3465A4 LINE2:errOut#CC0000",
"DEF:ucastIn=<FILE>:ucastPkIn:AVERAGE DEF:ucastOut=<FILE>:ucastPkOut:AVERAGE DEF:nucastIn:=<FILE>:nucastPkIn:AVERAGE DEF:nucastOut:=<FILE>:nucastPkOut:AVERAGE LINE1:ucastIn#3465A4 LINE2:ucastOut#CC0000 LINE3:nucastIn#ff0000 LINE4:nucastOut#00ff00"
    ]
).

generate_icmpProbe(ProbeId, Target) ->
    {ok, 
        #probe{
            id          = 0,
            name        = ProbeId,
            description = "icmp:Echo request",
            info        = "
                Trigger a single echo request every 30 seconds
            ",
            permissions = Target#target.global_perm,
            monitor_probe_mod   = bmonitor_probe_nagios,
            monitor_probe_conf  = #nagios_plugin_conf{
                 executable = "lib/go-check/build/go_check_icmp",
                 args       = ["-H", Target#target.ip, "-t", "5"],
                 eval_perfs = false
            },
            status      = 'UNKNOWN',
            timeout     = 5,
            %step        = 30,
            step        = 2,
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
    Ip        = proplists:get_value(ip,      Target#target.properties),
    case try_register_snmp_agent(Community, Ip) of
        {ok, _} ->
            generate_sysLocNameProbe(ProbeId, Target, Community);
        {error, Other} ->
            {error, Other}
    end.

generate_sysLocNameProbe(ProbeId, Target, Community) ->
    Community = proplists:get_value(snmp_ro, Target#target.properties),
    {ok, 
        #probe{
            id          = 1,
            name        = ProbeId,
            description = "snmp:sysName sysLocation",
            info        = "
                Set the target name and location properties depending on the
                MIB2 sysName and sysLocation OIDs every 10 minutes
            ",
            permissions = Target#target.global_perm,
            monitor_probe_mod   = bmonitor_probe_snmp,
            monitor_probe_conf  = #snmp_conf{
                port        = 161,
                version     = v2,
                community   = Community,
                oids        = [
                    {"sysName",     [1,3,6,1,2,1,1,5,0]},
                    {"sysLocation", [1,3,6,1,2,1,1,6,0]}
                ],
                method      = get
            },
            status      = 'UNKNOWN',
            timeout     = 5,
            step        = 600,
            inspectors  = [
                #inspector{
                    module  = bmonitor_inspector_status_set,
                    conf    = []
                },
                #inspector{
                    module  = bmonitor_inspector_property_set,
                    conf    = ["status", "sysName", "location"]
                    %% propagate will also set the target property defined:
                    %conf    = [
                        %"status",
                        %{propagate, "sysName"},
                        %{propagate, "sysLocation"}
                    %]
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
    case try_register_snmp_agent(Community, Ip) of
        {ok, TmpAgent} ->
            generate_ifPerfProbe(ProbeId, Target, Community, TmpAgent);
        {error, Other} ->
            {error, Other}
    end.

generate_ifPerfProbe(ProbeId, Target, Community, TmpAgent) ->
    Ifs         = snmp_manager:get_mib2_interfaces(TmpAgent),
    Ifs2        = filter_if_for_perfs(Ifs),
    Ifs3        = rename_if_needed(Ifs2),
    {QueryOids, RrdConf}   = generate_conf(Ifs3),
    {ok,
        #probe{
            id          = 2,
            name        = ProbeId,
            description = "snmp:ifTable performances",
            info        = "
            Query the element MIB-2 interface tree every 5 minutes and store 
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
                    [1,3,6,1,2,1,2,2,1,17],
                    [1,3,6,1,2,1,2,2,1,18],
                    [1,3,6,1,2,1,2,2,1,20]
                ]}
            },
            status      = 'UNKNOWN',
            timeout     = 5,
            step        = 300,
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
    IfOutNUcastPkts  = Descr ++ "_ifOutNUcastPkts",
    IfOutErrors      = Descr ++ "_ifOutErrors",
%     
    OidOctetsIn      = [1,3,6,1,2,1,2,2,1,10,Index,0],
    OidOctetsOut     = [1,3,6,1,2,1,2,2,1,16,Index,0],
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
            {IfOutUcastPkts,    "<UCAST-OUT>"},
            {IfOutNUcastPkts,   "<NUCAST-OUT>"},
            {IfOutErrors,       "<ERRORS-OUT>"}
        ],
        update_regexps  = none,
        file_path       = none
    },

    generate_conf(Ifs, {[Oids|OidsAcc], [OctetsInOutRrdConf|RrdsAcc]}).







try_register_snmp_agent(Community, Ip) ->
    % TODO handle SNMP v3
    TmpArgs = [
        {engine_id, "none"},
        {address,   Ip},
        {port,      161},
        {version,   v2},
        {community, Community}
    ],
    %TmpAgent    = snmp_manager:register_temporary_agent(TmpArgs),
    snmp_manager:register_temporary_agent(TmpArgs).

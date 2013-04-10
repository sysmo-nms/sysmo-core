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
-module(tracker_misc).
-behaviour(gen_server).
-include("../include/tracker.hrl").

-export([
    start_link/0,
    generate_id/0,
    fill_target_store/0,
    fill_target_more/0,
    fill_single/0,
    clear_target_store/0,
    some_ips/0,
    more_ips/0,
    valid_hostname_string/1,
    random/1,
    ip_format/2,
    extract_nag_uom/1,
    timestamp/1
]).

-export([
    init/1,
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

-record(state, {
    nagios_uom_re
}).
%%-------------------------------------------------------------
%% without this small server utility, random:uniform is called
%% at the same time at startup and return identical values.
%% It also keep state the compiled re:compile version for the 
%% nagios compat module
%%-------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

random(V) ->
    gen_server:call(?MODULE, {random, V}).

extract_nag_uom(String) ->
    gen_server:call(?MODULE, {extract_nag_uom, String}).

init([]) ->
    random:seed(),
    NagUomRe = [
        {usecond,       "us$"   },
        {msecond,       "ms$"   },
        {second,        "s$"    },
        {percent,       "%$"    },
        {kbytes,        "KB$"   },
        {mbytes,        "MB$"   },
        {tbytes,        "TB$"   },
        {bytes,         "B$"    },
        {counter,       "c$"    }
    ],

    NagCompiledRe = lists:map(fun({Unit, Re}) ->
        {ok, RE} = re:compile(Re),
        {Unit, RE}
    end, NagUomRe),
    {ok, #state{nagios_uom_re = NagCompiledRe}}.
    

handle_call({extract_nag_uom, String}, _F, S) ->
    Rep = nag_uom_test(String, S#state.nagios_uom_re),
    {reply, {ok, Rep}, S};

handle_call({random, V}, _F, S) ->
    {reply, random:uniform(V), S};

handle_call(_R, _F, S) ->
    {noreply, S}.


handle_cast(_R, S) ->
    {noreply, S}.

handle_info(_I, S) ->
    {noreply, S}.

terminate(_R, _S) ->
    normal.

code_change(_O, S, _E) ->
    {ok, S}.
%%-------------------------------------------------------------
%% end of gen_server
%%-------------------------------------------------------------

timestamp(second) ->
    {Meg, Sec, _} = erlang:now(),
    Meg * 1000000 + Sec;

timestamp(microsecond) ->
    {Meg, Sec, Mic} = erlang:now(),
    Meg * 1000000 * 1000000 + Sec * 1000000 + Mic.

nag_uom_test(String, []) ->
    {String, no_unit};

nag_uom_test(String, [{ReName, RE} | ReList]) ->
    case re:run(String, RE) of
        nomatch     -> 
            nag_uom_test(String, ReList);
        {match, _}  ->
            [Val, _] = re:replace(String, RE, ""),
            {erlang:binary_to_list(Val), ReName}
    end.




-spec generate_id() -> string().
% @doc generate_id. genere un ash pour le navigateur client
%		tips: pour etre sur de retourner une valeur unique on
%		peut utiliser le #user record + le erlang:universaltime.
%		L'existance du resultat dans mnesia est verifié. Si il
%		existe, il est relancé.
%. .   .   binary()
% @end
generate_id() ->
	generate_id({erlang:universaltime(), random:uniform()}).

generate_id(Term) ->
	Bin = erlang:md5(erlang:term_to_binary(Term)),
	List = erlang:binary_to_list(Bin),
	HexList = lists:map(fun(X) -> dec_to_hex(X) end, List),
	FinalList = concat_id(HexList),
	Id = list_to_binary(FinalList),
	erlang:list_to_atom("target-" ++ erlang:binary_to_list(Id)).

-spec fill_target_store() -> ok | any().
% @doc
% fill target_store with empty target records.
% @end
fill_target_store() ->
    SnmpConf = a_snmpv2_conf(),
    lists:foreach(fun(Ip) -> 
        New     = #target{},
        Prop    = New#target.properties,
        Prop1   = lists:keyreplace(ip, 1, Prop, {ip, Ip}),
        Prop2   = lists:keyreplace(snmp_conf, 1, Prop1, 
                {snmp_conf, SnmpConf#snmp_agent_conf{ip = Ip}}),
        NTarget = New#target{
            properties  = Prop2,
            probes      = [
                a_icmp_probe(),
                a_snmp_fetch_probe(),
                a_snmp_set_property_probe()
            ]
        },
        tracker_target_store:create_target(NTarget)
    end, some_ips()).

ip_format(v4_to_string, {Oa, Ob, Oc, Od}) ->
    String = io_lib:format(
            "~.10B~s~.10B~s~.10B~s~.10B",
            [Oa, ".", Ob, ".", Oc, ".", Od]),
    lists:flatten(String);


ip_format(v4_to_erlang, String)    ->
    {ok, Ip} = inet_parse:address(String),
    Ip.

fill_single() ->
    SnmpConf = a_snmpv2_conf(),
    lists:foreach(fun(Ip) -> 
        New     = #target{},
        Prop    = New#target.properties,
        Prop1   = lists:keyreplace(ip, 1, Prop, {ip, Ip}),
        Prop2   = lists:keyreplace(snmp_conf, 1, Prop1, 
                {snmp_conf, SnmpConf#snmp_agent_conf{ip = Ip}}),
        NTarget = New#target{
            properties  = Prop2,
            probes      = [
                a_icmp_probe(),
                a_snmp_fetch_probe(),
                a_snmp_set_property_probe(),
                a_icmp_probe_latency(),
                a_nagios_probe()
            ]
        },
        tracker_target_store:create_target(NTarget)
    end, [{192,168,1,2}]).

a_snmpv2_conf() ->
    #snmp_agent_conf{
        community   = "public",
        port        = 161,
        engine_id   = "none",
        version     = v2
    }.

a_nagios_probe() ->
    #probe{
            id  = 5,
            name = nagios_test,
            type = status,
            permissions = #perm_conf{
                read = ["admin"],
                write = ["admin"]
            },
            tracker_probe_mod   = btracker_probe_nagios_compat,
            tracker_probe_conf  = #nagios_plugin{
                executable  = "./lib/tracker/priv/nagios/libexec/check_ping",
                args        = [
                    {"-H", {target, {properties, ip}}},
                    {"-w", "3000,50%"},
                    {"-c", "4000,50%"},
                    {"-t", {probe, timeout}}
                ]
            },
            inspectors  = [
                #inspector{
                    module  = btracker_inspector_simple,
                    conf    = []
                }
            ],
            timeout = 10,
            step    = 5,


            loggers      = [
                #logger{
                    module  = btracker_logger_file, 
                    conf    = []
                }
            ]
        }.

a_icmp_probe() ->
    #probe{
            id  = 4,
            name = icmp_test,
            type = status,
            permissions = #perm_conf{
                read = ["admin"],
                write = ["admin"]
            },
            tracker_probe_mod = btracker_probe_icmp_echo,
            inspectors  = [
                #inspector{
                    module  = btracker_inspector_simple,
                    conf    = []
                }
            ],
            timeout = 10,
            step    = 5,


            loggers      = [
                #logger{
                    module  = btracker_logger_file, 
                    conf    = []
                }
            ]
        }.

a_snmp_fetch_probe() ->
    #probe{
            id  = 3,
            name = snmp_fetch_test,
            type = fetch,
            permissions = #perm_conf{
                read = ["admin"],
                write = ["admin"]
            },
            tracker_probe_mod = btracker_probe_snmp,
            inspectors  = [
                #inspector{
                    module  = btracker_inspector_simple,
                    conf    = []
                }
            ],
            timeout = 10,
            step    = 5,


            snmp_oids   = [
                [1,3,6,1,2,1,1,7,0]
            ],

            loggers = [
                #logger{
                    module  = btracker_logger_file, 
                    conf    = []
                },
                #logger{
                    module  = btracker_logger_rrd, 
                    conf    = #rrd_def{
                        rrd_update = #rrd_update{
                            file    = "snmp_fetch_test-2.rrd",
                            time    = now,
                            updates = [
                                #rrd_ds_update{
                                    name    = "bytes",
                                    value   = 0
                                }
                            ]
                        },
            
                        rrd_create = #rrd_create{
                            file        = "snmp_fetch_test-2.rrd",
                            start_time  = undefined,
                            step        = 5,
                            ds_defs     = [
                                #rrd_ds{
                                    name    = "bytes",
                                    type    = gauge,
                                    heartbeat = 25,
                                    min     = 0,
                                    max     = 100000000,
                                    args    = "25:0:U"
                                }
                            ],
                            rra_defs    = [
                                #rrd_rra{cf = 'max', args = "0:1:3600"},
                                    #rrd_rra{cf = 'max', args = "0:12:1440"}
                            ]
                        },
        
                        rrd_graph = "graph"
                    }
                }
            ]
        }.

a_icmp_probe_latency() ->
    #probe{
            id  = 2,
            name = icmp_latency_test,
            type = fetch,
            permissions = #perm_conf{
                read = ["admin"],
                write = ["admin"]
            },
            tracker_probe_mod = btracker_probe_snmp,
            inspectors  = [
                #inspector{
                    module  = btracker_inspector_simple,
                    conf    = []
                }
            ],
            timeout = 10,
            step    = 5,

            loggers = [
                #logger{
                    module  = btracker_logger_file, 
                    conf    = []
                },
                #logger{
                    module  = btracker_logger_rrd, 
                    conf    = #rrd_def{
                        rrd_update = #rrd_update{
                            file    = "icmp_latency_test-2.rrd",
                            time    = now,
                            updates = [
                                #rrd_ds_update{
                                    name    = "latency",
                                    value   = 0
                                }
                            ]
                        },
            
                        rrd_create = #rrd_create{
                            file        = "icmp_latency_test-2.rrd",
                            start_time  = undefined,
                            step        = 5,
                            ds_defs     = [
                                #rrd_ds{
                                    name    = "latency",
                                    type    = gauge,
                                    heartbeat = 25,
                                    min     = 0,
                                    max     = 100000000,
                                    args    = "25:0:U"
                                }
                            ],
                            rra_defs    = [
                                #rrd_rra{cf = 'max', args = "0:1:3600"},
                                    #rrd_rra{cf = 'max', args = "0:12:1440"}
                            ]
                        },
        
                        rrd_graph = "graph"
                    }
                }
            ]
        }.

a_snmp_set_property_probe() ->
    #probe{
            id  = 1,
            name = snmp_set_property_test,
            type = {property, sysname},
            permissions = #perm_conf{
                read = ["admin"],
                write = ["admin"]
            },
            tracker_probe_mod = btracker_probe_snmp,
            inspectors  = [
                #inspector{
                    module  = btracker_inspector_simple,
                    conf    = []
                }
            ],
            
            loggers         = [
                #logger{
                    module  = btracker_logger_file,
                    conf    = []
                }
            ],

            snmp_oids = [
                [1,3,6,1,2,1,1,5,0]
            ],

            timeout = 10,
            step    = 5
        }.

fill_target_more() ->
    lists:foreach(fun(X) -> tracker_target_store:new(X) end, more_ips()).

clear_target_store() ->
    lists:foreach(fun(X) ->
        tracker_target_store:del_target(X)
    end, tracker_target_store:get_ids()).




% @private
%%--------------------------------------------------------------
%% @doc fonctions pour formater le nouveau id
%% @end
%%--------------------------------------------------------------
dec_to_hex(Dec) ->
	Rem = Dec rem 16,
	Div = Dec div 16,
	dec_to_hex(Div, [map_dec_to_hex(Rem)]).

dec_to_hex(0, List) ->
	List;

dec_to_hex(Dec, List) ->
	Rem = Dec rem 16,
	Div = Dec div 16,
	dec_to_hex(Div, [map_dec_to_hex(Rem) | List]).

map_dec_to_hex(Dec) ->
	case Dec of
		0 -> $0;
		1 -> $1;
		2 -> $2;
		3 -> $3;
		4 -> $4;
		5 -> $5;
		6 -> $6;
		7 -> $7;
		8 -> $8;
		9 -> $9;
		10 -> $A;
		11 -> $B;
		12 -> $C;
		13 -> $D;
		14 -> $E;
		15 -> $F
	end.

concat_id([H | T]) ->
	concat_id(T, H).

concat_id([], Final) ->
	Final;

concat_id([A | B], Final) ->
	concat_id(B, A ++ Final).


some_ips() ->
    [
        {192,168,1,2},
        {173,194,34,63},
        {173,194,34,56},
        {173,194,34,55},
        {212,27,48,10},
        {77,238,178,122},
        {77,238,178,122},
        {87,248,120,148}
    ].

more_ips() ->
    [
        {192,168,1,2},
        {74,125,230,0},
        {74,125,230,1},
        {74,125,230,2},
        {74,125,230,3},
        {74,125,230,4},
        {74,125,230,5},
        {74,125,230,6},
        {74,125,230,7},
        {74,125,230,8},
        {74,125,230,9},
        {74,125,230,10},
        {74,125,230,11},
        {74,125,230,12},
        {74,125,230,13},
        {74,125,230,14},
        {74,125,230,15},
        {74,125,230,16},
        {74,125,230,17},
        {74,125,230,18},
        {74,125,230,19},
        {74,125,230,20},
        {74,125,230,21},
        {74,125,230,22},
        {74,125,230,23},
        {74,125,230,24},
        {74,125,230,25},
        {74,125,230,26},
        {74,125,230,27},
        {74,125,230,28},
        {74,125,230,29},
        {74,125,230,30},
        {74,125,230,31},
        {74,125,230,32},
        {74,125,230,33},
        {74,125,230,34},
        {74,125,230,35},
        {74,125,230,36},
        {74,125,230,37},
        {74,125,230,38},
        {74,125,230,39},
        {74,125,230,40},
        {74,125,230,41},
        {74,125,230,42},
        {74,125,230,43},
        {74,125,230,44},
        {74,125,230,45},
        {74,125,230,46},
        {74,125,230,47},
        {74,125,230,48},
        {74,125,230,49},
        {74,125,230,50},
        {74,125,230,51},
        {74,125,230,52},
        {74,125,230,53},
        {74,125,230,54},
        {74,125,230,55},
        {74,125,230,56},
        {74,125,230,57},
        {74,125,230,58},
        {74,125,230,59},
        {74,125,230,60},
        {74,125,230,61},
        {74,125,230,62},
        {74,125,230,63},
        {74,125,230,192},
        {74,125,230,193},
        {74,125,230,194},
        {74,125,230,195},
        {74,125,230,196},
        {74,125,230,197},
        {74,125,230,198},
        {74,125,230,199},
        {74,125,230,200},
        {74,125,230,201},
        {74,125,230,202},
        {74,125,230,203},
        {74,125,230,204},
        {74,125,230,205},
        {74,125,230,206},
        {74,125,230,207},
        {74,125,230,208},
        {74,125,230,209},
        {74,125,230,210},
        {74,125,230,211},
        {74,125,230,212},
        {74,125,230,213},
        {74,125,230,214},
        {74,125,230,215},
        {74,125,230,216},
        {74,125,230,217},
        {74,125,230,218},
        {74,125,230,219},
        {74,125,230,220},
        {74,125,230,221},
        {74,125,230,222},
        {74,125,230,223},
        {74,125,230,224},
        {74,125,230,225},
        {74,125,230,226},
        {74,125,230,227},
        {74,125,230,228},
        {74,125,230,229},
        {74,125,230,230},
        {74,125,230,231},
        {74,125,230,232},
        {74,125,230,233},
        {74,125,230,234},
        {74,125,230,235},
        {74,125,230,236},
        {74,125,230,237},
        {74,125,230,238},
        {74,125,230,239},
        {74,125,230,240},
        {74,125,230,241},
        {74,125,230,242},
        {74,125,230,243},
        {74,125,230,244},
        {74,125,230,245},
        {74,125,230,246},
        {74,125,230,247},
        {74,125,230,248},
        {74,125,230,249},
        {74,125,230,250},
        {74,125,230,251},
        {74,125,230,252},
        {74,125,230,253},
        {74,125,230,254},
        {74,125,230,255}
    ].
        
valid_hostname_string(Arg) ->
    Fun = fun(X) ->
        if 
            X >= 48, X =< 90    -> true; 
            X >= 97, X =< 122   -> true;
            X == 45             -> true;
            X == 95             -> true;
            true                -> false
        end
    end,
    case is_list(Arg) of
        true ->
            lists:all(Fun, Arg);
        false ->
            false
    end.

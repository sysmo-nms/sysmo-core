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
-module(bmonitor_probe_nagios).
-behaviour(beha_monitor_probe).
-include("include/monitor.hrl").

-export([
    init/2,
    exec/1,
    info/0
]).

-record(state, {
    nag_re,
    exec,
    args,
    eval_perfs
}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% beha_monotor_probe callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
info() ->
    {ok, "Nagios plugin compatible probe"}.

init(_Target, Probe) ->
    Conf = Probe#probe.monitor_probe_conf,

    {ok, NagRe} = compile_nagios_re(),
    {ok, #state{
            nag_re      = NagRe,
            exec        = Conf#nagios_probe_conf.executable,
            args        = Conf#nagios_probe_conf.args,
            eval_perfs  = Conf#nagios_probe_conf.eval_perfs
        }
    }.

exec(State) ->
    Args            = State#state.args,
    Exec            = State#state.exec,
    Evaluate        = State#state.eval_perfs,
    Re              = State#state.nag_re,
    
    {_, MicroSec1}  = sys_timestamp(),

    erlang:open_port({spawn_executable, Exec}, 
        [exit_status, stderr_to_stdout, {args, Args}]),

    case receive_port_info() of
        {0, Stdout} ->
            PR = evaluate_nagios_output(Evaluate, Stdout, Re, 'OK');
        {1, Stdout} ->
            PR = evaluate_nagios_output(Evaluate, Stdout, Re, 'WARNING');
        {2, Stdout} ->
            PR = evaluate_nagios_output(Evaluate, Stdout, Re, 'CRITICAL');
        {3, Stdout} ->
            PR = evaluate_nagios_output(Evaluate, Stdout, Re, 'UNKNOWN');
        {Any, Stdout} ->
            io:format("Other return status~p~n", [Any]),
            PR = evaluate_nagios_output(Evaluate, Stdout, Re, 'UNKNOWN')
    end,
    {_, MicroSec2} = sys_timestamp(),
    KV  = PR#probe_return.key_vals,
    KV2 = [{"sys_latency", MicroSec2 - MicroSec1} | KV],
    PR2 = PR#probe_return{
        timestamp   = MicroSec2,
        key_vals    = KV2
    },
    {ok, State, PR2}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTILS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
receive_port_info() ->
    receive_port_info("").
receive_port_info(StdOut) ->
    receive
        {_, {exit_status, Status}} ->
            {Status, StdOut};
        {_, {data, StdOutData}} ->
            receive_port_info(lists:append(StdOut, StdOutData));
        _ ->
            receive_port_info(StdOut)
    end.

% @private
evaluate_nagios_output(false, StdOutput, _, Status) ->
    #probe_return{
        status          = Status,
        original_reply  = StdOutput,
        key_vals        = [{"status", Status}]
    };

evaluate_nagios_output(true, StdOutput, Re, Status) ->

    {_, PerfLines} = lists:foldr(fun(X, {TextOut, Perfs}) ->
        case string:tokens(X, "|") of
            [Text] ->
                {[Text | TextOut], Perfs};
            [Text, Perf] ->
                {[Text | TextOut], [Perf | Perfs]}
        end
    end, {[], []}, string:tokens(StdOutput, "\n")),

    % generate a list of perfs from lines
    PerfStringList = lists:foldl(fun(PerfLine, Acc) ->
        PerfsOnLine = string:tokens(PerfLine, " "),
        lists:append(PerfsOnLine, Acc)
    end, [], PerfLines),

    % generate [#nagios_perf_data{}]
    KeyValList   = lists:foldl(fun(PerfElement, Acc) ->
        PerfDataList = string:tokens(PerfElement, ";"),
        case PerfDataList of
            [LabelValue, Warn, Crit, Min, Max] ->
                [Label, ValueUom]   = string:tokens(LabelValue, "="),
                {ok, {Value, Uom}}  = nag_uom_test(ValueUom, Re),
                Vars = [
                    {string:concat(Label, "_value"), to_number(Value)},
                    {string:concat(Label, "_uom"),   to_number(Uom)},
                    {string:concat(Label, "_warn"),  to_number(Warn)},
                    {string:concat(Label, "_crit"),  to_number(Crit)},
                    {string:concat(Label, "_min"),   to_number(Min)},
                    {string:concat(Label, "_max"),   to_number(Max)}
                ],
                lists:concat([Acc, Vars]);
            [LabelValue, Warn, Crit, Min] ->
                [Label, ValueUom] = string:tokens(LabelValue, "="),
                {ok, {Value, Uom}}  = nag_uom_test(ValueUom, Re),
                Vars = [
                    {string:concat(Label, "_value"), to_number(Value)},
                    {string:concat(Label, "_uom"),   to_number(Uom)},
                    {string:concat(Label, "_warn"),  to_number(Warn)},
                    {string:concat(Label, "_crit"),  to_number(Crit)},
                    {string:concat(Label, "_min"),   to_number(Min)}
                ],
                lists:concat([Acc, Vars]);
            [LabelValue, Warn, Crit] ->
                [Label, ValueUom] = string:tokens(LabelValue, "="),
                {ok, {Value, Uom}}  = nag_uom_test(ValueUom, Re),
                Vars = [
                    {string:concat(Label, "_value"), to_number(Value)},
                    {string:concat(Label, "_uom"),   to_number(Uom)},
                    {string:concat(Label, "_warn"),  to_number(Warn)},
                    {string:concat(Label, "_crit"),  to_number(Crit)}
                ],
                lists:concat([Acc, Vars]);
            [LabelValue, Warn] ->
                [Label, ValueUom] = string:tokens(LabelValue, "="),
                {ok, {Value, Uom}}  = nag_uom_test(ValueUom, Re),
                Vars = [
                    {string:concat(Label, "_value"), to_number(Value)},
                    {string:concat(Label, "_uom"),   to_number(Uom)},
                    {string:concat(Label, "_warn"),  to_number(Warn)}
                ],
                lists:concat([Acc, Vars]);
            [LabelValue] ->
                [Label, ValueUom] = string:tokens(LabelValue, "="),
                {ok, {Value, Uom}}  = nag_uom_test(ValueUom, Re),
                Vars = [
                    {string:concat(Label, "_value"), to_number(Value)},
                    {string:concat(Label, "_uom"),   to_number(Uom)}
                ],
                lists:concat([Acc, Vars])
        end
    end, [], PerfStringList),

    % is there some datas?
    case KeyValList of
        []  ->
            #probe_return{
                status          = Status,
                original_reply  = StdOutput,
                key_vals        = [{"status", Status}]
            };
        _   ->
            #probe_return{
                status          = Status,
                original_reply  = StdOutput,
                key_vals        = [{"status", Status} | KeyValList]
            }
    end.

to_number(String) ->
    to_number(String, [list_to_float, list_to_integer]).
to_number(String, []) ->
    String;
to_number(String, [ToSomething | T]) ->
    case (catch erlang:ToSomething(String)) of
        {'EXIT', _} ->
            to_number(String, T);
        Other ->
            Other
    end.

sys_timestamp() ->
    {Meg, Sec, Micro} = os:timestamp(),
    Seconds  = Meg * 1000000 + Sec,
    Microsec = Seconds * 1000000 + Micro,
    {Seconds, Microsec}.

compile_nagios_re() ->
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
    {ok, NagCompiledRe}.

nag_uom_test(String, []) ->
    {String, no_unit};

nag_uom_test(String, [{ReName, Re} |ReList]) ->
    case re:run(String, Re) of
        nomatch ->
            nag_uom_test(String, ReList);
        {match, _} ->
            [Val, _] = re:replace(String, Re, ""),
            {ok, {erlang:binary_to_list(Val), ReName}}
    end.

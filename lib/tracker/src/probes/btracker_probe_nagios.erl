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
-module(btracker_probe_nagios).
-behaviour(beha_tracker_probe).
-include("../../include/tracker.hrl").

-export([
    exec/1,
    info/0
]).

exec({_, #probe{
            tracker_probe_conf  = #nagios_plugin_conf{
                executable  = Exec,
                args        = Args
            }
        }
    }) ->
 
    ArgList = [erlang:tuple_to_list(X) || X <- Args],

    erlang:open_port({spawn_executable, Exec}, 
        [exit_status, {args, ArgList}]),

    case receive_port_info() of
        {0, Stdout} ->
            PR =  evaluate_nagios_output(Stdout),
            PR#probe_return{status = 'OK'};
        {1, Stdout} ->
            PR =  evaluate_nagios_output(Stdout),
            PR#probe_return{status = 'WARNING'};
        {2, Stdout} ->
            PR =  evaluate_nagios_output(Stdout),
            PR#probe_return{status = 'CRITICAL'};
        {3, Stdout} ->
            PR =  evaluate_nagios_output(Stdout),
            PR#probe_return{status = 'UNKNOWN'};
        {Any, Stdout} ->
            io:format("Other return status~p~n", [Any]),
            PR =  evaluate_nagios_output(Stdout),
            PR#probe_return{status = 'UNKNOWN'}
    end.

receive_port_info() ->
    receive_port_info("").
receive_port_info(StdOut) ->
    receive
        {_, {exit_status, Status}} ->
            {Status, StdOut};
        {_, {data, StdOutData}} ->
            receive_port_info(StdOutData);
        _ ->
            receive_port_info(StdOut)
    end.

info() ->
    {ok, "Nagios plugin compatible probe"}.

% @private
evaluate_nagios_output(StdOutput) ->

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
                {ok, {Value, Uom}}  = tracker_misc:extract_nag_uom(ValueUom),
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
                {ok, {Value, Uom}}  = tracker_misc:extract_nag_uom(ValueUom),
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
                {ok, {Value, Uom}}  = tracker_misc:extract_nag_uom(ValueUom),
                Vars = [
                    {string:concat(Label, "_value"), to_number(Value)},
                    {string:concat(Label, "_uom"),   to_number(Uom)},
                    {string:concat(Label, "_warn"),  to_number(Warn)},
                    {string:concat(Label, "_crit"),  to_number(Crit)}
                ],
                lists:concat([Acc, Vars]);
            [LabelValue, Warn] ->
                [Label, ValueUom] = string:tokens(LabelValue, "="),
                {ok, {Value, Uom}}  = tracker_misc:extract_nag_uom(ValueUom),
                Vars = [
                    {string:concat(Label, "_value"), to_number(Value)},
                    {string:concat(Label, "_uom"),   to_number(Uom)},
                    {string:concat(Label, "_warn"),  to_number(Warn)}
                ],
                lists:concat([Acc, Vars]);
            [LabelValue] ->
                [Label, ValueUom] = string:tokens(LabelValue, "="),
                {ok, {Value, Uom}}  = tracker_misc:extract_nag_uom(ValueUom),
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
                original_reply  = StdOutput,
                timestamp       = tracker_misc:timestamp(second)
            };
        _   ->
            #probe_return{
                original_reply  = StdOutput,
                key_vals        = KeyValList,
                timestamp       = tracker_misc:timestamp(second)
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

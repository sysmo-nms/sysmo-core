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
-module(bmonitor_probe_ncheck).
-behaviour(beha_monitor_probe).
-include("include/monitor.hrl").

-export([
    init/2,
    exec/1,
    info/0
]).

-record(state, {
    exec,
    args,
    eval_perfs
}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% beha_monotor_probe callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
info() ->
    {ok, "Noctopus checks"}.

init(_Target, Probe) ->
    Conf = Probe#probe.monitor_probe_conf,
    {ok, #state{
            exec        = Conf#ncheck_probe_conf.executable,
            args        = Conf#ncheck_probe_conf.args,
            eval_perfs  = Conf#ncheck_probe_conf.eval_perfs
        }
    }.

exec(State) ->
    Args            = State#state.args,
    Exec            = State#state.exec,
    Evaluate        = State#state.eval_perfs,
    
    {_, MicroSec1}  = sys_timestamp(),

    erlang:open_port({spawn_executable, Exec}, 
        [exit_status, stderr_to_stdout, {args, Args}]),

    case receive_port_info() of
        {0, Stdout} ->
            PR = evaluate_ncheck_output(Evaluate, Stdout);
        {Exit, Stdout} ->
            io:format("Other return status~p~n", [Exit]),
            PR = #probe_return{
                status          = 'ERROR',
                original_reply  = Stdout
            }
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
            receive_port_info(StdOutData);
        _ ->
            receive_port_info(StdOut)
    end.

% @private
sys_timestamp() ->
    {Meg, Sec, Micro} = os:timestamp(),
    Seconds  = Meg * 1000000 + Sec,
    Microsec = Seconds * 1000000 + Micro,
    {Seconds, Microsec}.

evaluate_ncheck_output(_Eval, Stdout) ->
    % voir doc xmerl_scan
    %A = xmerl_scan:string(Stdout),
    #probe_return{
        status          = 'UNKNOWN',
        original_reply  = Stdout
    }.

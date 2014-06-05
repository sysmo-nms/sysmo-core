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
-include_lib("xmerl/include/xmerl.hrl").

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
            receive_port_info(lists:append(StdOut, StdOutData));
        R ->
            ?LOG({what, R, StdOut}),
            receive_port_info(StdOut)
    end.

% @private
sys_timestamp() ->
    {Meg, Sec, Micro} = os:timestamp(),
    Seconds  = Meg * 1000000 + Sec,
    Microsec = Seconds * 1000000 + Micro,
    {Seconds, Microsec}.

evaluate_ncheck_output(_Eval, Stdout) ->
    ?LOG(Stdout),
    % XXX Sdtout is sometime "\n"?
    C = (catch xmerl_scan:string(Stdout, [{space,normalize}])),
    case C of
        {'EXIT', Error} ->
            error_logger:info_msg("~p ~p: error: ~p~n",[?MODULE,?LINE,Error]),
            #probe_return{
                status          = 'UNKNOWN',
                original_reply  = Stdout
            };
        {X,_R} ->

            %Attrs       = X#xmlElement.attributes,
            %NameAttr    = lists:keyfind(name, 2, Attrs),
            %Name        = NameAttr#xmlAttribute.value,
            Contents    = lists:filter(fun(E) ->
                case E of
                    #xmlElement{} = E -> true;
                    _                 -> false
                end
            end ,X#xmlElement.content),
            %Command     = lists:keyfind(command,        2, Contents),
            Status      = lists:keyfind(status,         2, Contents),
            TextOut     = lists:keyfind(text_output,    2, Contents),
            %_Perfs      = lists:keyfind(perfs,          2, Contents),
        
            %[C] = Command#xmlElement.content,
            [S] = Status#xmlElement.content,
            [T] = TextOut#xmlElement.content,
            #probe_return{
                status          = erlang:list_to_existing_atom(S#xmlText.value),
                original_reply  = T#xmlText.value
            }
    end.

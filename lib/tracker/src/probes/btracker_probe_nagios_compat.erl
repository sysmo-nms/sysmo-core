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
-module(btracker_probe_nagios_compat).
-behaviour(beha_tracker_probe).
-include("../../include/tracker.hrl").
-include_lib("snmp/include/snmp_types.hrl").

-export([
    exec/1,
    info/0
]).

exec({  
        #target{
            directory           = Dir
        } = Target, 
        #probe{
            name                = Name, 
            id                  = ProbeId,
            tracker_probe_conf  = #nagios_plugin{
                executable  = Exec,
                args        = Args
            }
        } = Probe
    }) ->
    FileName    = io_lib:format("~s-~.10B~s", [Name, ProbeId, ".stdout"]),
    StdoutFile  = filename:absname_join(Dir, FileName),

    Command = lists:foldl(fun({Flag, Value}, Acc) ->
        case is_list(Value) of
            true    ->
                lists:concat([Acc, " ", Flag, " ", Value]);
            false   ->
                lists:concat([Acc, " ", Flag, " ", get_val(Value, Target, Probe)])
        end
    end, Exec, Args),
    
    Id = self(),
    spawn(fun() ->
        process_flag(trap_exit, true),
        exec:run_link(Command, 
                [{stdout, StdoutFile}]),
        receive
            A -> Id ! A
        end
    end),

    io:format("jjjjjjjjjj ~p~n", [StdoutFile]),
    receive
        {_, _, {exit_status, Val}} -> 
            case exec:status(Val) of
                {status, 0} ->
                    io:format("nagios_compat received exit 0~p~n", [return_string(StdoutFile)]),
                    {'OK',          return_string(StdoutFile)};
                {status, 1} ->
                    io:format("nagios_compat received exit 1~p~n", [return_string(StdoutFile)]),
                    {'WARNING',     return_string(StdoutFile)};
                {status, 2} ->
                    io:format("nagios_compat received exit 2~p~n", [return_string(StdoutFile)]),
                    {'CRITICAL',    return_string(StdoutFile)};
                {status, 3} ->
                    io:format("nagios_compat received exit 3~p~n", [return_string(StdoutFile)]),
                    {'UNKNOWN',     return_string(StdoutFile)};
                Any -> 
                    io:format("uuuuuuuuuuuuuuuuuuuuuuuuu~p~n", [Any])
            end;
        Any ->
            io:format("ddddddddddddddddddddddddd~p~n" , [Any])
    end.

return_string(File) ->
    {ok, Data} = file:read_file(File),
    Data.

info() ->
    {ok, "Nagios compat probe module"}.

% @private
get_val({target, {properties, ip}}, #target{properties = Props}, _) ->
    {ip, Val} = lists:keyfind(ip, 1, Props),
    tracker_misc:ip_format(v4_to_string, Val);

get_val({probe, timeout}, _, #probe{timeout = Timeout}) ->
    lists:flatten(io_lib:format("~.10B", [Timeout])).


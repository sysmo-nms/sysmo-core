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
-module(probe_nchecks).
-behaviour(monitor_exec).
-include("include/monitor.hrl").
-include("../nchecks/include/nchecks.hrl").

-export([
    init/1,
    exec/1,
    info/0
]).

-record(state, {
    class,
    args
}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% monotor_probe callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
info() ->
    {ok, "Sysmo checks"}.

init(Probe) ->
    [Target]      = monitor_data_master:get(target, Probe#probe.belong_to),
    ?LOG(Target),
    TargetProp  = Target#target.properties,
    Conf        = Probe#probe.monitor_probe_conf,
    #nchecks_probe_conf{class = Class, args = Args} = Conf,

    % if "host" is not defined in probe conf, use the target "ip" and
    % "ipVersion" property.
    case proplists:lookup("host", Args) of
        none ->
            TargHost = proplists:lookup("host", TargetProp),
            NewArgs  = [TargHost|Args];
        _ ->
            NewArgs = Args
    end,
    {ok,
        #state{
            class = Class,
            args  = NewArgs
        }
    }.


exec(#state{class = Class, args = Args} = S) ->
    case nchecks:check(Class,Args) of
        {error, Error} ->
            ProbeReturn = #probe_return{
                status      = "ERROR",
                reply_string   = Error
            };
        {ok, Reply} ->
            #nchecks_reply{
               status=Status,performances=Perfs,reply_string=Str,timestamp=Ts
            } = Reply,
            ProbeReturn = #probe_return{
                status          = Status,
                reply_string    = Str,
                timestamp       = Ts,
                key_vals        = Perfs
            }
    end,
    {ok, S, ProbeReturn}.

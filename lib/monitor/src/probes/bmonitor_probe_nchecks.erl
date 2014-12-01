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
-module(bmonitor_probe_nchecks).
-behaviour(monitor_exec).
-include("include/monitor.hrl").
-include("../nchecks/include/nchecks.hrl").

-export([
    init/2,
    exec/1,
    info/0
]).

-record(state, {
    function,
    args
}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% monotor_probe callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
info() ->
    {ok, "Noctopus checks"}.

init(Target, Probe) ->
    TargetProp  = Target#target.properties,
    Conf        = Probe#probe.monitor_probe_conf,
    #nchecks_probe_conf{function = Funct, args = Args} = Conf,

    % if "host" is not defined in probe conf, use the target "ip" and
    % "ipVersion" property.
    case proplists:lookup("host", Args) of
        none ->
            Args1   = proplists:delete("ipVersion", Args),
            TargIp  = proplists:get_value("ip", TargetProp),
            TargIpV = proplists:lookup("ipVersion", TargetProp),
            NewProp = [{"host", TargIp}, TargIpV],
            NewArgs = lists:append([NewProp,Args1]);
        _ ->
            NewArgs = Args
    end,
    {ok,
        #state{
            function = Funct,
            args     = NewArgs
        }
    }.


exec(#state{function = Funct, args = Args} = S) ->
    case nchecks:Funct(Args) of
        {error, Error} ->
            ProbeReturn = #probe_return{
                status      = "ERROR",
                original_reply = Error
            };
        {ok, Reply} ->
            #nchecks_reply{
               status=Status,performances=Perfs,reply_string=Str,timestamp=Ts
            } = Reply,
            ProbeReturn = #probe_return{
                status          = Status,
                original_reply  = Str,
                timestamp       = Ts,
                key_vals        = Perfs
            }
    end,
    {ok, S, ProbeReturn}.

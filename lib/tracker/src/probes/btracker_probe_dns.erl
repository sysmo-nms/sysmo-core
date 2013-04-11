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
-module(btracker_probe_dns).
-behaviour(beha_tracker_probe).
-include("../../include/tracker.hrl").
-export([
    exec/1,
    info/0
]).

% icmp_server:ping(Ip, Timeout) -> must return {ok, Val} | {error, Error}
exec({#target{properties = Prop}, #probe{timeout = Timeout}}) ->
    {ip, Ip} = lists:keyfind(ip, 1, Prop),
    case inet_res:gethostbyaddr(Ip, Timeout) of
        {ok, #hostent{h_name = ResolvedName}} = R -> 
            #probe_return{
                status          = 'OK',
                timestamp       = tracker_misc:timestamp(second),
                original_reply  = R,
                key_val         = {hostname, ResolvedName}
            };
        {error, _} = R -> 
            #probe_return{
                status = 'ERROR',
                timestamp       = tracker_misc:timestamp(second),
                original_reply  = R
            }
    end.

info() ->
    {ok, ""}.

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
-module(btracker_probe_snmp).
-behaviour(beha_tracker_probe).
-include("../../include/tracker.hrl").
-include_lib("snmp/include/snmp_types.hrl").

-export([
    exec/1,
    info/0
]).

exec({#target{id = Id}, #probe{snmp_oids = Oids, timeout = Timeout}}) ->
    Rep = esnmp_user_v2:sync_get(
        atom_to_list(Id),
        Oids,
        Timeout * 1000
    ),

    case Rep of
        {ok, {noError, _, Reply}, _} ->

            Ret = lists:foldl(fun(X, Acc) ->
                [{X#varbind.oid, X#varbind.value} | Acc]
            end, [], Reply),
            #probe_return{
                status          = 'OK',
                timestamp       = tracker_misc:timestamp(second),
                key_vals        = Ret,
                original_reply  = Rep
            };
        {error, _} ->
            #probe_return{
                status          = 'ERROR',
                timestamp       = tracker_misc:timestamp(second),
                original_reply  = Rep
            };
        Other ->
            io:format("Other ~p~n", [Other])
    end.
    

info() ->
    {ok, "Snmp probe module"}.

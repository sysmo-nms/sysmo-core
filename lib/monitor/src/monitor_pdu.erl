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
% @doc
% @end
-module(monitor_pdu).
-include("include/monitor.hrl").

-export([
    probe_return/1
]).

probe_return({
        #probe_return{ 
            status      = Status,
            original_reply = OriginalReply,
            timestamp   = Timestamp,
            key_vals    = KeyVals
        },
        ChannelId, ProbeId}) ->
    {modMonitorPDU,
        {fromServer,
            {probeReturn,
                {'ProbeReturn',
                    atom_to_list(ChannelId),
                    atom_to_list(ProbeId),
                    atom_to_list(Status),
                    OriginalReply,
                    Timestamp,
                    make_key_values(KeyVals)
                }}}}.

make_key_values(K) ->
    make_key_values(K, []).
make_key_values([], S) ->
    S;
make_key_values([{K,V} | T], S) when is_list(V) ->
    make_key_values(T, [{'Property', K, V} | S]);
make_key_values([{K,V} | T], S) when is_integer(V) ->
    make_key_values(T, [{'Property', K, integer_to_list(V)} | S]);
make_key_values([{K,V} | T], S) when is_float(V) ->
    make_key_values(T, [{'Property', K, float_to_list(V, [{decimals, 10}])} | S]);
make_key_values([{K,V} | T], S) when is_atom(V) ->
    make_key_values(T, [{'Property', K, atom_to_list(V)} | S]).

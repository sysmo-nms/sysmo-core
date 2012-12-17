% This file is part of "Enms" (http://sourceforge.net/projects/enms/)
% Copyright (C) 2012 <Sébastien Serre sserre.bx@gmail.com>
% 
% Enms is a Network Management System aimed to manage and monitor SNMP
% targets, monitor network hosts and services, provide a consistent
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
-module(esnmp_api_ifs).
-behaviour(beha_ifs_module).
-include_lib("../../pdu-0.1.0/build/ModEsnmp.hrl").

% beha_ifs_module export
-export([handle_msg/2, pre_process/1, initial_conn/1]).
% @doc
% Handle a command from a client
% @end
-spec esnmp_api_ifs:handle_msg(Data::term(), ClientState::record()) -> 
        AsnResponce::term() | noreply.
handle_msg(Msg, ClientState) ->
    io:format("~p: Message ~p from ~p~n", [?MODULE, Msg, ClientState]),
    noreply.

% @doc
% Return roles and valid asn term.
% @end
-spec esnmp_api_ifs:pre_process(Data::term()) -> 
        {Asn::tuple(), Roles::list(Role::string())}.
pre_process({trap, {v2_community, Community}, {Ip, Port}, Payload}) ->
    Roles = community_to_roles(Community),
    %Tags  = esnmp_conf:get_tags(Source),
    Tags = ["a","b"],
    Msg = {modEsnmpPDU, {fromServer, {trap, [#'TrapsTableRow'{
        timeStamp       = "time",
        version         = lists:flatten(io_lib:format("~p", [v2])),
        fromIp          = lists:flatten(io_lib:format("~p", [Ip])),
        fromPort        = lists:flatten(io_lib:format("~p", [Port])),
        credentials     = lists:flatten(io_lib:format("~p", [Roles])),
        tags            = lists:flatten(io_lib:format("~p", [Tags])),
        message         = lists:flatten(io_lib:format("~p", [Payload])) }]}}},
    {Roles, Msg}.

-spec esnmp_api_ifs:initial_conn(ClientState::record()) -> 
        {term(), Roles::list(Role::string())}.
initial_conn(ClientState) ->
    io:format("~p initialconn!!!! from ~p~n", [?MODULE, ClientState]),
    ok.

community_to_roles(_Community) ->
    ["admin"].

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
% beha_ifs_module export
-export([handle_msg/2, present/1, initial_conn/1]).

% @doc
% Handle a command from a client
% @end
-spec esnmp_api_ifs:handle_msg(Data::term(), ClientState::record()) -> any().
handle_msg(Msg, ClientState) ->
    io:format("~p: Message ~p from ~p~n", [?MODULE, Msg, ClientState]).

% @doc
% Callback from ifs module. Data must be presented so it can
% be compiled with asnc, and fetch the roles for wich the
% @end
-spec esnmp_api_ifs:present(Data::term()) -> {Asn::tuple(), Roles::list(Role::string())}.
present(Term) ->
    io:format("present Term ~n"),
    {Term, ["admin"]}.

-spec esnmp_api_ifs:initial_conn(ClientState::record()) -> {term(), Roles::list(Role::string())}.
initial_conn(_ClientState) ->
    ok.

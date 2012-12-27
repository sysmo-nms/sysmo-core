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
% @private
-module(ifs_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    {ok, AuthModule}      = application:get_env(mod_ifs, ifs_auth),
    {ok, TcpClientConf}   = application:get_env(mod_ifs, tcp_client),
    {ok, SslClientConf}   = application:get_env(mod_ifs, ssl_client),
    io:format("~n~p: authmod: ~p~n", [?MODULE,AuthModule]),
    io:format("~n~p: tcp_client: ~p~n", [?MODULE,TcpClientConf]),
    io:format("~n~p: ssl_client: ~p~n", [?MODULE,SslClientConf]),
    ifs_sup:start_link(AuthModule, TcpClientConf, SslClientConf).

stop(_State) ->
	ok.
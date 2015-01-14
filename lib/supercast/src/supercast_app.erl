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
-module(supercast_app).
-behaviour(application).
-include("../yaws/include/yaws.hrl").

-export([start/2, stop/1]).

start(_Type, _Args) ->
    configure_yaws(),
    supercast_sup:start_link().

configure_yaws() ->
    {ok, YawsConf} = application:get_env(supercast, http_conf),
    {ok, DocRoot}  = application:get_env(supercast, http_sync_dir),
    {ok, Port}     = application:get_env(supercast, http_port),
    LogDir      = proplists:get_value(logdir, YawsConf),
    Listen      = proplists:get_value(listen, YawsConf),
    CompLevel   = proplists:get_value(compression_level, YawsConf),

    GcList = [
        {id, "supercast"},
        {logdir, LogDir}
    ],
    SConfList = [
        {port, Port},
        {servername, "supercast"},
        {listen, Listen},
        {appmods, [{"/interface", mod_pyfcgi}]},
        {fcgi_app_server, {"localhost",8777}},
        {docroot, DocRoot},
        {deflate_options,
            {deflate,
                nolimit,CompLevel,-15,8,
                default,false,all
            }
        }
    ],
    {ok,[[Sc]],Gc,_} = yaws_api:embedded_start_conf(DocRoot, SConfList, GcList),
    UpSc = ?sc_set_deflate(Sc, true),
    yaws_api:setconf(Gc, [[UpSc]]).

stop(_State) ->
	ok.

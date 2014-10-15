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

-define(COMPRESSIBLE_MIME_TYPES, [
                                    {"text", all},
                                    {"application", "rtf"},
                                    {"application", "msword"},
                                    {"application", "postscript"},
                                    {"application", "pdf"},
                                    {"application", "x-dvi"},
                                    {"application", "javascript"},
                                    {"application", "x-javascript"},
                                    {"application", "xml"}
                                ]).


-export([start/2, stop/1]).

start(_Type, _Args) ->
    configure_yaws(),
    {ok, AuthModule}    = application:get_env(supercast, auth_module),
    {ok, AcctrlMod}     = application:get_env(supercast, acctrl_module),
    {ok, TcpClientConf} = application:get_env(supercast, tcp_client),
    {ok, SslClientConf} = application:get_env(supercast, ssl_client),
    {ok, PduDispatch}   = application:get_env(supercast, pdu_dispatch),
    {ok, MainChannels}  = application:get_env(supercast, main_channels),
    supercast_sup:start_link(
        {AuthModule, PduDispatch},              % for supercast_server
        {AcctrlMod, MainChannels},              % for supercast_mpd
        TcpClientConf,                          % for tcp_client_sup
        SslClientConf).                         % for ssl_client_sup

configure_yaws() ->
    {ok, YawsConf} = application:get_env(supercast, yaws_conf),
    Port        = proplists:get_value(port, YawsConf),
    LogDir      = proplists:get_value(logdir, YawsConf),
    Listen      = proplists:get_value(listen, YawsConf),
    CompLevel   = proplists:get_value(compression_level, YawsConf),
    DocRoot     = proplists:get_value(docroot, YawsConf),

    GcList = [
        {id, "supercast"},
        {logdir, LogDir}
    ],
    SConfList = [
        {port, Port},
        {servername, "supercast"},
        {listen, Listen},
        {docroot, DocRoot},
        {deflate_options,
            {deflate,
                nolimit,CompLevel,-15,8,
                default,false,?COMPRESSIBLE_MIME_TYPES
            }
        }
    ],
    {ok,Sc,Gc,_} = yaws_api:embedded_start_conf(DocRoot, SConfList, GcList),
    yaws_api:setconf(Gc, Sc).

stop(_State) ->
	ok.

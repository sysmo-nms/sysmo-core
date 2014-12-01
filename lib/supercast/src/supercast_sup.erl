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
-module(supercast_sup).
-behaviour(supervisor).

-export([start_link/4]).
-export([init/1]).

start_link(SrvConf, MpdConf, TcpClientConf, SslClientConf) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, 
            [SrvConf, MpdConf, TcpClientConf, SslClientConf]).

init([SrvConf, MpdConf, TcpClientConf, SslClientConf]) ->
    SupercastRegistrar = {
        supercast_registrar,
        {supercast_registrar,start_link, []},
        permanent,
        2000,
        worker,
        [supercast_registrar]
    },

    SupercastServer = {
        supercast_server,
        {supercast_server,start_link, [SrvConf]},
        permanent,
        2000,
        worker,
        [supercast_server]
    },
    SupercastMpdSup = {
        supercast_mpd_sup,
        {supercast_mpd_sup,start_link, [MpdConf, TcpClientConf, SslClientConf]},
        permanent,
        infinity,
        supervisor,
        [supercast_mpd_sup]
    },

    {ok,
        {
            {one_for_one, 1, 300},
            [SupercastRegistrar, SupercastServer, SupercastMpdSup]
        }
    }.

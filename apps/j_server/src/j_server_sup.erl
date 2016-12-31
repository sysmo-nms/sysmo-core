%%==============================================================================
%% Sysmo NMS Network Management and Monitoring solution (http://www.sysmo.io)
%%
%% Copyright (c) 2012-2017 Sebastien Serre <ssbx@sysmo.io>
%%
%% Sysmo NMS is free software: you can redistribute it and/or modify it under
%% the terms of the GNU General Public License as published by the Free Software
%% Foundation, either version 3 of the License, or (at your option) any later
%% version.
%%
%% Sysmo NMS is distributed in the hope that it will be useful, but WITHOUT ANY
%% WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
%% A PARTICULAR PURPOSE. See the GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License along with
%% Sysmo.  If not, see <http://www.gnu.org/licenses/>.
%%==============================================================================
% @private
-module(j_server_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok,
        {
            {one_for_all, 0, 6000},
            [
                {
                    j_server,
                    {j_server, start_link, []},
                    permanent,
                    15000,
                    worker,
                    [j_server]
                },
                {
                    j_server_nchecks,
                    {j_server_nchecks, start_link, []},
                    permanent,
                    2000,
                    worker,
                    [j_server_nchecks]
                },
                {
                    j_server_errd4j,
                    {j_server_errd4j, start_link, []},
                    permanent,
                    2000,
                    worker,
                    [j_server_errd4j]
                },
                {
                    j_server_snmpman,
                    {j_server_snmpman, start_link, []},
                    permanent,
                    2000,
                    worker,
                    [j_server_snmpman]
                },
                {
                    j_server_eventdb,
                    {j_server_eventdb, start_link, []},
                    permanent,
                    2000,
                    worker,
                    [j_server_eventdb]
                }
            ]
        }
    }.

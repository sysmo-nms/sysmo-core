%%==============================================================================
%% Sysmo NMS Network Management and Monitoring solution (http://www.sysmo.io)
%%
%% Copyright (c) 2012-2016 Sebastien Serre <ssbx@sysmo.io>
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
-module(sysmo_sup).
-behaviour(supervisor).

-export([
    start_link/0
]).
-export([init/1]).

start_link() ->
    case mnesia:system_info(use_dir) of
        false -> mnesia:create_schema([node()]);
        true  -> ok
    end,
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok,
        {
            {one_for_all, 1, 6000},
            [
                {
                    mnesia_sup,
                    {mnesia_sup, start, [normal,[]]},
                    permanent,
                    2000,
                    supervisor,
                    [mnesia_sup]
                },
                {
                    j_server_app,
                    {j_server_app, start, [normal,[]]},
                    permanent,
                    2000,
                    supervisor,
                    [j_server_app]
                },
                {
                    supercast_app,
                    {supercast_app, start, [normal,[]]},
                    permanent,
                    2000,
                    supervisor,
                    [supercast_app]
                },
                { %% only important is monitor last
                    monitor_app,
                    {monitor_app, start, [normal,[]]},
                    permanent,
                    2000,
                    supervisor,
                    [monitor_app]
                }
            ]
        }
    }.

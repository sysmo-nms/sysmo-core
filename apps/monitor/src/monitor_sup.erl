%%==============================================================================
%% Sysmo NMS Network Management and Monitoring solution (https://sysmo-nms.github.io)
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
-module(monitor_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-include("monitor.hrl").

start_link() ->
    ets:new(?ETS_PROBES_REGISTER, [set, public, named_table,
        {write_concurrency, false}, {read_concurrency, true}, {keypos, 1}]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok,
        {
            {one_for_all, 0, 6000},
            [
                {
                    monitor_events,
                    {monitor_events, start_link, []},
                    permanent,
                    infinity,
                    worker,
                    [monitor_events]
                },
                {
                    monitor_scheduler,
                    {monitor_scheduler, start_link, []},
                    permanent,
                    infinity,
                    worker,
                    [monitor_scheduler]
                },
                {
                    nchecks_probe_sup,
                    {nchecks_probe_sup, start_link, []},
                    permanent,
                    infinity,
                    supervisor,
                    [nchecks_probe_sup]
                },
                {
                    monitor_controller,
                    {monitor_controller, start_link, []},
                    permanent,
                    2000,
                    worker,
                    [monitor_controller]
                },
                {
                    monitor_data_master,
                    {monitor_data_master, start_link, []},
                    permanent,
                    infinity,
                    worker,
                    [monitor_data_master]
                },
                {
                    monitor_channel,
                    {monitor_channel, start_link, []},
                    permanent,
                    2000,
                    worker,
                    [monitor_channel]
                }
            ]
        }
    }.

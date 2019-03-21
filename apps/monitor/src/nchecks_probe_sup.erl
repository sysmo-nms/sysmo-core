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
-module(nchecks_probe_sup).
-behaviour(supervisor).
-include("monitor.hrl").

-export([start_link/0, launch/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

launch(Probe) ->
    {ok, _} = supervisor:start_child(?MODULE, [Probe]).

init([]) ->
    {ok,
        {
            {simple_one_for_one, 1, 60},
            [
                {
                    nchecks_probe,
                    {nchecks_probe, start_link, []},
                    transient,
                    2000,
                    worker,
                    [nchecks_probe]
                }
            ]
        }
    }.


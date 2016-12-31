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
%% @private
-module(sysmo_app).
-behaviour(application).
-include_lib("kernel/include/file.hrl").

-export([start/2, start_phase/3, stop/1]).

start(_Type, _Args) ->
    sysmo_sup:start_link().

start_phase(listen, _Type, _Args) -> supercast:listen().

stop(_State) ->
    ok.

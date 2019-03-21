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
% DATA SOURCE TYPE
-define(DS_GAUGE,   "GAUGE").
-define(DS_COUNTER, "COUNTER").
-define(DS_DERIVE,  "DERIVE").
-define(DS_ABSOLUTE,"ABSOLUTE").

% CONSOLIDATION FUNCTION
-define(CF_AVERAGE, "AVERAGE").
-define(CF_MIN,     "MIN").
-define(CF_MAX,     "MAX").
-define(CF_LAST,    "LAST").
-define(CF_FIRST,   "FIRST").
-define(CF_TOTAL,   "TOTAL").

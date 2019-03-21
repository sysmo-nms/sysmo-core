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
-ifdef(debug).
-define(LOG_INFO(String,Term),
    error_logger:info_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String},
       {term, Term}])).

-define(LOG_INFO(String),
    error_logger:info_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String}])).

-define(LOG_WARNING(String,Term),
    error_logger:warning_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String},
       {term, Term}])).


-define(LOG_WARNING(String),
    error_logger:warning_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String}])).
-else.
-define(LOG_WARNING(String), ok).
-define(LOG_WARNING(String,Term), ok).
-define(LOG_INFO(String), ok).
-define(LOG_INFO(String,Term), ok).
-endif.



-define(LOG_ERROR(String,Term),
    error_logger:error_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String},
       {term, Term}])).

-define(LOG_ERROR(String),
    error_logger:error_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String}])).

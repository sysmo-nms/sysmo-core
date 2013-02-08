% This file is part of "Enms" (http://sourceforge.net/projects/enms/)
% Copyright (C) 2012 <Sébastien Serre sserre.bx@gmail.com>
% 
% Enms is a Network Management System aimed to manage and monitor SNMP
% target, monitor network hosts and services, provide a consistent
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
-module(tracker_events).
-export([start_link/1]).

% @doc
% Start the event manager and initialise the module.
% @end
-spec esnmp_events:start_link(list()) -> {ok, pid()}.
start_link(GenEventListeners) ->
    % START the event manager:
    ReturnSup = gen_event:start_link({local, ?MODULE}),

    lists:foreach(fun(X) -> 
        gen_event:add_handler(?MODULE, X, tracker)
    end, GenEventListeners),

    ReturnSup.

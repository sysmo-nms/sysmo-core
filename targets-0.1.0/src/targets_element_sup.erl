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
-module(targets_element_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link([TargetRecord]) ->
    supervisor:start_link(?MODULE, [TargetRecord]).

init([TargetRecord]) ->
    {ok, 
        {
            {one_for_one, 1, 60},
            [
                {
                    targets_element,
                    {targets_element, start_link, [TargetRecord]},
                    permanent,
                    2000,
                    worker,
                    [targets_element]
               },
               {
                    targets_element_probe_dock,
                    {targets_element_probe_dock, start_link, []},
                    permanent,
                    infinity,
                    supervisor,
                    [targets_element_probe_dock]
               }
            ]
        }
    }.

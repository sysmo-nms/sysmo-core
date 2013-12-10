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
-module(tracker_events_manager).
-include("../include/tracker.hrl").

-export([
    start_link/0,
    add_handler/2,
    notify/1,
    call/2,
    call/3,
    delete_handler/2,
    which_handlers/0
]).


start_link() ->
    gen_event:start_link({local, ?MODULE}).

add_handler(Module, Args) ->
    gen_event:add_handler(?MODULE, Module, Args).

notify(Event) ->
    gen_event:notify(?MODULE, Event).

call(Handler, Request) ->
    gen_event:call(?MODULE, Handler, Request).

call(Handler, Request, Timeout) ->
    gen_event:call(?MODULE, Handler, Request, Timeout).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?MODULE, Handler, Args).

which_handlers() ->
    gen_event:which_handlers(?MODULE).

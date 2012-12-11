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
-module(esnmp_modsrv_events).
-export([start_link/0, modsrv_notify/1]).

% @doc
% Start the event manager and initialise the module.
% @end
-spec esnmp_events:start_link() -> {ok, Pid::pid()}.
start_link() ->
    % START the event manager:
    ReturnSup = gen_event:start_link({local, ?MODULE}),
    % add a default handler for debug:
    gen_event:add_handler(?MODULE, esnmp_terminal_logger, []),
    % notify the availability of ?MODULE to modsrv
    modsrv:hello({mod_esnmp, [
        {modsrv_callback, ?MODULE}, 
        {event_handler, ?MODULE},
        {ifs_callback, esnmp_api_ifs},
        {ifs_asnkey, modEsnmpPDU}]}),

    ReturnSup.

% @doc
% Action to be taken when a module appear or disapear.
% ?MODULE did not depend on any modules so do nothing
% @end
-spec esnmp_modsrv:modsrv_notify(Any::any()) -> ok.
modsrv_notify(_Args) ->
    ok.

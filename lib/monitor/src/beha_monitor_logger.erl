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
% @doc
% The module implementing this behaviour is used by a monitor_target_channel
% to store values returned by the probes.
% @end
-module(beha_monitor_logger).
-include("../include/monitor.hrl").

-export([behaviour_info/1]).

% exported here for documentation only:
-export([
    init/3,
    log/2,
    dump/1
]).

% @private
behaviour_info(callbacks) ->
    [
        {init, 3},
        {log,  2},
        {dump, 1}
    ];

behaviour_info(_) ->
    undefined.

-spec init(Conf::[any()], Dir::string(), Probe::#probe{}) 
        -> {ok, any()}.
% @doc
% Called at the target_target_channel probe initialisation.
% @end
init(_Conf, _Dir, _Probe) -> 
    {ok, undefined}.


-spec log(ProbeServerState::#ps_state{}, Msg::any()) 
        -> ok.
% @doc
% Called each time a message responce from the probe fun is received.
% Is it to this module to be sure that a log action is not pending when
% a ?MODULE:dump/2 occur using locks or other.
% This function is called from the channel. The channel can call dump/2
% after. It is to the module to define when a log is pending before
% sending a dump using a gen_server or other lock method.
% @end
log(_ProbeServerState, _Msg) ->
    ok.

-spec dump(ProbeServerState::#ps_state{}) -> 
    {ok, binary()} | ignore | timeout.
% @doc
% Called by a monitor_target_channel on a subscribe request by a client. Must
% return a binary form of the data logged, ignore or timeout.
% For synchronisation, the monitor_target_channel server will wait for a
% a responce. The function MUST return before some kind of timeout return
% it is reached a buggy logger which lock indefinetely will result in a 
% crash of the channel.
% @end
dump(_ProbeServerState) ->
    ignore.

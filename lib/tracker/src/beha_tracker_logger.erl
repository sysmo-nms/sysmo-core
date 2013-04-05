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
% The module implementing this behaviour is used by a tracker_target_channel
% to store values returned by the probes.
% @end
-module(beha_tracker_logger).
-include("../include/tracker.hrl").

-export([behaviour_info/1]).

% exported here for documentation only:
-export([
    init/3,
    log/4,
    dump/4
]).

% @private
behaviour_info(callbacks) ->
    [
        {init, 3},
        {log,  4},
        {dump, 4}
    ];

behaviour_info(_) ->
    undefined.

-spec init(Conf::[any()], Target::#target{}, Probe::#probe{}) -> ok.
% @doc
% Called at the target_target_channel probe initialisation.
% @end
init(_Conf, _Target, _Probe) -> 
    ok.

-spec log(Conf::[any()], Target::#target{}, Probe::#probe{}, Msg::any()) -> ok.
% @doc
% Called each time a message responce from the probe fun is received.
% @end
log(_Conf, _Target, _Probe, Msg) ->
    io:format("msg is ~p~n", [Msg]),
    ok.

-spec dump(Conf::[any()], Target::#target{}, Probe::#probe{}, Timeout::integer()) -> 
    {ok, binary()} | ignore.
% @doc
% Called by a tracker_target_channel on a subscribe request by a client. Must
% return a binary form of the data logged or ignore.
% For synchronisation, the tracker_target_channel server will wait for a
% a responce. The function MUST return before Timeout or return ignore.
% Take care, a bugy function here can lock indefinetely wish result in a 
% crash of the channel.
% @end
dump(_Conf, _Target, _Probe, _Timeout) ->
    ignore.

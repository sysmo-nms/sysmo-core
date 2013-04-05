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
% The most simple inspector. If the return is not {ok,_}, then the status
% of the probe is set to 'NOTICE'. Else it is 'OK'.
% @end
-module(btracker_inspector_simple).
-behaviour(beha_tracker_inspector).
-include("../include/tracker.hrl").


-export([
    init/2, 
    inspect/3,
    info/0
]).

info() ->
    [
        'NOTICE',
        'OK'
    ].

init(_Conf, ProbeServerState) -> 
    {ok, ProbeServerState}.

% @end
inspect(_InitS, #probe_server_state{
        probe = Probe} = ProbeServerState, {ok, _}) ->
    NewProbe = Probe#probe{status = 'OK'},
    {ok, ProbeServerState#probe_server_state{probe = NewProbe}};

inspect(_InitS, #probe_server_state{
        probe               = Probe} = ProbeServerState, {error, _}) ->
    NewProbe = Probe#probe{status = 'NOTICE'},
    {ok, ProbeServerState#probe_server_state{probe = NewProbe}}.

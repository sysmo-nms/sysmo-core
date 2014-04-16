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
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with Enms. If not, see <http://www.gnu.org/licenses/>.
% @doc
% The most simple and mandatory inspector. It set re return status of the
% ProbeServer to the return status of the probe return.
% @end
-module(btracker_inspector_parent).
-behaviour(beha_tracker_inspector).
-include("../../include/tracker.hrl").


-export([
    init/2,
    inspect/3,
    info/0
]).

info() ->
    [].

init(_, PS) ->
    {ok, PS}.

% @end
inspect(_InitPSState, ModifiedPSState, _ProbeReturn) ->
    #ps_state{probe = Probe}    = ModifiedPSState,
    #probe{name     = Name}     = Probe,  
    #probe{status   = Status}   = Probe,

    case Status of
        'OK' ->         % nothing to do
            io:format("is ok~n"),
            {ok, ModifiedPSState};
        'UNKNOWN' ->    % nothing to do
            io:format("is unknown~n"),
            {ok, ModifiedPSState};
        'WARNING' ->    % nothing to do
            {ok, ModifiedPSState};
        'CRITICAL' ->   % must test something
            tracker_probe_fsm:critical_return(Name),
            TmpProbe = Probe#probe{status = 'UNKNOWN'},
            TmpState = ModifiedPSState#ps_state{probe = TmpProbe},
            {ok, TmpState}
    end.


% UTILS btracker_inspector_parent

% UTILS beha_tracker_probe
% store_state(Value, 
%         #ps_state{inspectors_state = IState} = ProbeServerState) ->
%     IConf = lists:keystore(?MODULE, 1, IState, {?MODULE, Value}),
%     ProbeServerState#ps_state{inspectors_state = IConf}.
% 
% get_state(#ps_state{inspectors_state = IS}) ->
%     {?MODULE, Value} = lists:keyfind(?MODULE, 1, IS),
%     Value.

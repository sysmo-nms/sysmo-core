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
-module(tracker_api).
-include("../include/tracker.hrl").
-compile(export_all).

handle_command({fromClient, {createTarget, Msg}}, 
        #client_state{module = CMod} = CState) ->
    {'TargetCreate', Ip, Hostname, Sysname, Read, Write, CmdId} = Msg,
    ReadPerm = case Read of
        [] ->
            ["admin"];
        _  ->
            [Read]
    end,
    WritePerm = case Write of
        [] ->
            ["admin"];
        _ ->
            [Write]
    end,
    case inet_parse:address(Ip) of
        {ok, EIp}   ->
            launch_target(#target{
                id          = tracker_misc:generate_id(),
                ip          = EIp,
                hostname    = Hostname,
                sysname     = Sysname,
                global_perm = #perm_conf{
                                read    = ReadPerm,
                                write   = WritePerm
                                }  
            }, CState, CmdId);
        {error, _}  ->
            CMod:send(CState, pdu(comResp, {CmdId, "Bad ip format"}))
    end;

handle_command({fromClient, {createProbe, Msg}}, 
        #client_state{module = _CMod} = _CState) ->
    io:format("createProbe ~p~n",[Msg]);

handle_command({fromClient, Other}, _) ->
    io:format("Unknown command ~p~n", [Other]).

pdu(comResp, {CmdId, CmdMsg}) ->
    {modTrackerPDU,
        {fromServer,
            {cmdResp,
                {'CommandResponce',
                    CmdId,
                    CmdMsg
    }   }   }   }.


launch_target(Target, #client_state{module = CMod} = CState, CmdId) ->
    case tracker_target_channel_sup:new(Target) of
        {ok, _} ->
            CMod:send(CState, pdu(comResp, {CmdId, "ok"}));
        _ ->
            CMod:send(CState, pdu(comResp, {CmdId, "error"}))
    end.

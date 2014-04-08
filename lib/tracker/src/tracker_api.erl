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
-behaviour(gen_commander).
-include("include/tracker.hrl").
-export([
    handle_command/2
]).

% TODO put it in a gen_server loop and use cast to free supercast_server
% and correctly handle random targetIds (random:uniform(10000000000) * 99999).
%handle_command({modTrackerPDU, {fromClient, {createTarget, Msg}}}, CState) ->
    %{Status, Info} = create_target(Msg),
    %send(CState, pdu(trackerReply, {QueryId, true, "hello from server!!!!"}));

handle_command({modTrackerPDU, {fromClient, Msg}}, CState) ->
    {_, {_, _, _, QueryId}} = Msg,
    send(CState, pdu(trackerReply, {QueryId, true, "hello from server!!!!"})),
    io:format("received ~p~n", [Msg]).

send(#client_state{module = CMod} = CState, Msg) ->
    CMod:send(CState, Msg).

pdu(trackerReply, {QueryId, Status, Info}) ->
    {modTrackerPDU,
        {fromServer,
            {trackerReply,
                {'TrackerReply',
                    QueryId,
                    Status,
                    Info }}}}.

% handle_command({fromClient, {createProbe, Msg}}, 
%         #client_state{module = _CMod} = _CState) ->
%     io:format("createProbe ~p~n",[Msg]);
% 
% handle_command({fromClient, {createTarget, Msg}}, 
%         #client_state{module = CMod} = CState) ->
%     {'TargetCreate', 
%         Ip, Hostname, 
%         Sysname, 
%         {'PermConf', Read, Write},
%         CmdId
%     } = Msg,
%     ReadPerm = case Read of
%         [] ->
%             ["admin"];
%         _  ->
%             ["admin"| Read]
%     end,
%     WritePerm = case Write of
%         [] ->
%             ["admin"];
%         _ ->
%             ["admin"| Write]
%     end,
%     case inet_parse:address(Ip) of
%         {ok, EIp}   ->
%             launch_target(#target{
%                 id          = tracker_misc:generate_id(),
%                 properties = [
%                     {ip             , EIp},
%                     {hostname       , Hostname},
%                     {sysname        , Sysname},
%                     {global_perm    , 
%                         #perm_conf{
%                             read    = ReadPerm,
%                             write   = WritePerm
%                         }
%                     }  
% 
%                 ]
%             }, CState, CmdId);
%         {error, _}  ->
%             CMod:send(CState, pdu(comResp, {CmdId, "ERROR: Bad ip format"}))
%     end;
% 
% 
% handle_command({fromClient, Other}, _) ->
%     io:format("Unknown command ~p~n", [Other]).
% 
% 
% 
% % UTILS
% launch_target(Target, #client_state{module = CMod} = CState, CmdId) ->
%     case tracker_target_channel_sup:new(Target) of
%         {ok, Pid} ->
%             Info = erlang:process_info(Pid),
%             {registered_name, RegName} = 
%                 lists:keyfind(registered_name, 1, Info),
%             Rep = lists:append("OK: ", atom_to_list(RegName)),
%             io:format("rep is ~p~n", [Rep]),
%             CMod:send(CState, pdu(comResp, {CmdId, Rep}));
%         Other ->
%             Rep = lists:append("ERROR: ", atom_to_list(Other)),
%             CMod:send(CState, pdu(comResp, {CmdId, Rep}))
%     end.

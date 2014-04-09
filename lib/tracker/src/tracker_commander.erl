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
-module(tracker_commander).
-behaviour(supercast_commander).
-behaviour(gen_server).
-include("include/tracker.hrl").
-export([
    start_link/0,
    handle_command/2
]).

-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

% 1 000 000 possible values
-define(RAND_RANGE, 1000000).
% but must be a minimum of 100000
-define(RAND_MIN,   99999).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

handle_command(Command, CState) ->
    gen_server:cast(?MODULE, {command, Command, CState}).

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% INIT
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
init([]) -> 
    {ok, no_commander_state}.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% HANDLE_CALL 
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
handle_call(_R, _F, S) ->
    {noreply, S}.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% HANDLE_CAST
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
handle_cast({command, {modTrackerPDU, {fromClient, Command}}, CState}, S) ->
    {_, {_,_,_,QueryId}} = Command,
    {ok, Info}           = generate_id("target-"),
    Pdu                  = pdu(trackerReply, {QueryId, true, Info}),
    send(CState, Pdu),
    {noreply, S};
handle_cast(_R, S) ->
    io:format("unknown cast ~p~n", [_R]),
    {noreply, S}.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% HANDLE_INFO
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
handle_info(_I, S) ->
    {noreply, S}.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% TERMINATE  
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
terminate(R, State) ->
    ok = tracker_master_channel:chan_del(State),
    R.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% CODE_CHANGE
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
code_change(_O, S, _E) ->
    {ok, S}.



%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% UTILS
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
generate_id(Head) ->
    Int         = random:uniform(?RAND_RANGE),
    RandId      = Int + ?RAND_MIN,
    RandIdL     = io_lib:format("~p", [RandId]),
    RandIdS     = lists:flatten(RandIdL),
    RandIdF     = lists:concat([Head, RandIdS]),
    {ok, RandIdF}.

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






%%----------------------------------------------------------------------------
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


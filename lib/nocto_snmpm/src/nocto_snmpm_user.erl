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
-module(nocto_snmpm_user).
-behaviour(snmpm_user).
-include_lib("snmp/include/snmp_types.hrl").
-include("include/nocto_snmpm.hrl").

%% BEHAVIOUR snmpm_user exports
-export([
    handle_error/3,
    handle_agent/5,
    handle_pdu/4,
    handle_trap/3,
    handle_inform/3,
    handle_report/3
]).

%% API exports
-export([
    which_agents/0,
    get_mib2_system/1,
    get_mib2_interfaces/1
]).

%% BEHAVIOUR snmpm_user
handle_error(_ReqId, _Reason, _UserData) ->
    io:format("handle_error ~p~n", [?MODULE]),
    ignore.

handle_agent(_Addr, _Port, _Type, _SnmpInfo, _UserData) ->
    io:format("handle_agent ~p~n", [?MODULE]),
    ignore.

handle_pdu(_TargetName, _ReqId, SnmpResponse, _UserData) ->
    io:format("handle_pdu ~p ~p~n", [?MODULE,SnmpResponse]),
    ignore.

handle_trap(_TargetName, _SnmpTrapInfo, _UserData) ->
    io:format("handle_trap ~p~n", [?MODULE]),
    ignore.

handle_inform(_TargetName, _SnmpInform, _UserData) ->
    io:format("handle_inform ~p~n", [?MODULE]),
    ignore.

handle_report(_TargetName, _SnmpReport, _UserData) ->
    io:format("handle_report ~p~n", [?MODULE]),
    ignore.

%% API
which_agents() ->
    snmpm:which_agents(?SNMPM_USER).

get_mib2_system(Agent) ->
    Reply = snmpm:sync_get(?SNMPM_USER, Agent, [
        ?OID_SYS_DESCR,
        ?OID_SYS_OBJECT_ID,
        ?OID_SYS_UPTIME,
        ?OID_SYS_CONTACT,
        ?OID_SYS_NAME,
        ?OID_SYS_LOCATION,
        ?OID_SYS_SERVICES
    ]),

    case Reply of
        {ok, {noError,0, Responce}, _} ->
            {varbind,_,_,SysDescr,_}
                = lists:keyfind(?OID_SYS_DESCR,     2, Responce),
            {varbind,_,_,SysObjectId,_}
                = lists:keyfind(?OID_SYS_OBJECT_ID, 2, Responce),
            {varbind,_,_,SysUptime,_}
                = lists:keyfind(?OID_SYS_UPTIME,    2, Responce),
            {varbind,_,_,SysContact,_}
                = lists:keyfind(?OID_SYS_CONTACT,   2, Responce),
            {varbind,_,_,SysName,_}
                = lists:keyfind(?OID_SYS_NAME,      2, Responce),
            {varbind,_,_,SysLocation,_}
                = lists:keyfind(?OID_SYS_LOCATION,  2, Responce),
            {varbind,_,_,SysServices,_}
                = lists:keyfind(?OID_SYS_SERVICES,  2, Responce),

            #mib2_system{
                sys_descr       = SysDescr,
                sys_object_id   = SysObjectId,
                sys_uptime      = SysUptime,
                sys_contact     = SysContact,
                sys_name        = SysName,
                sys_location    = SysLocation,
                sys_services    = SysServices
            }
    end.

get_mib2_interfaces(Agent) ->
    _IfIndexes      = sync_walk_bulk(Agent, ?OID_IF_INDEX),
    _IfDescr        = sync_walk_bulk(Agent, ?OID_IF_DESCR),
    _IfType         = sync_walk_bulk(Agent, ?OID_IF_TYPE),
    _IfMTU          = sync_walk_bulk(Agent, ?OID_IF_MTU),
    _IfSpeed        = sync_walk_bulk(Agent, ?OID_IF_SPEED),
    _IfPhysAdd      = sync_walk_bulk(Agent, ?OID_IF_PHYS_ADDRESS),
    _IfAdminStatus  = sync_walk_bulk(Agent, ?OID_IF_ADMIN_STATUS),
    _IfOperStatus   = sync_walk_bulk(Agent, ?OID_IF_OPER_STATUS),
    _IfLastChange   = sync_walk_bulk(Agent, ?OID_IF_LAST_CHANGE),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SNMP BULK WALK IMPLEMENTATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sync_walk_bulk(Agent, Oid) ->
    sync_walk_bulk(Agent, Oid, Oid, []).
sync_walk_bulk(Agent, StartOID, LastOID, Result) ->
    Reply = snmpm:sync_get_bulk(
        ?SNMPM_USER, Agent, 0, ?BULK_MAX_REP, [LastOID]
    ),
    case Reply of
        {ok, {noError,_,R}, _} ->
            {_,Last,_,_,_} = lists:last(R),
            case still_in_tree(StartOID, Last) of
                true  -> 
                    sync_walk_bulk(
                        Agent, StartOID, Last, lists:append([Result,R])
                    );
                false ->    
                    % TODO remove out of StartOid responces
                    lists:append([Result, R])
            end;
        _ ->
            []
    end.

still_in_tree(Tree,Oid) when length(Tree) > length(Oid) ->
    false;
still_in_tree([], _Oid) -> 
    true;
still_in_tree([TH|TreeOid] , [OH|Oid]) when TH == OH ->
    still_in_tree(TreeOid, Oid);
still_in_tree(_,_) ->
    false.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -record(interface, {
%     if_index            = 0             :: integer(),
%     if_descr            = ""            :: string(),
%     if_type             = 0             :: integer(),
%     if_mtu              = 0             :: integer(),
%     if_speed            = 0             :: integer(),
%     if_phys_address     = []            :: [integer()]
% }).
 
% -record(state, {
%     agents              = []            :: [#agent{}]
% }).

% initialize_agent(Agent) ->
%     Reply = snmpm:sync_get(?SNMP_USER, Agent, 
%         [?SYS_NAME_OID, ?SYS_SERVICES_OID, ?IF_NUMBER_OID]
%     ),
%     case Reply of
%         {ok, {noError, _, Rep}, _} ->
%             {_,_,_,SysName,_}   = lists:keyfind(?SYS_NAME_OID,      2, Rep),
%             {_,_,_,SysServ,_}   = lists:keyfind(?SYS_SERVICES_OID,  2, Rep),
%             {_,_,_,IfNumber,_}  = lists:keyfind(?IF_NUMBER_OID,     2, Rep),
%             Interfaces = initialize_interfaces(IfNumber, Agent),
%             #agent{
%                 name            = Agent,
%                 sys_name        = SysName,
%                 sys_services    = SysServ,
%                 if_number       = IfNumber,
%                 interfaces      = Interfaces
%             };
%         _ ->
%             ?LOG({reply_error, Agent, Reply})
%     end.
% 
% initialize_interfaces(IfNumber, Agent) ->
%     {ok, {noError, _, IfEntrysRem0}, _} = snmpm:sync_get_bulk(
%         ?SNMP_USER, Agent, 0, IfNumber * 6,[
%             [1,3,6,1,2,1,2,2,1]
%         ]
%     ),
%     {IfIndexes, IfEntrysRem1}  = lists:split(IfNumber, IfEntrysRem0),
%     {_IfDescr,   IfEntrysRem2}  = lists:split(IfNumber, IfEntrysRem1),
%     {_IfType,    IfEntrysRem3}  = lists:split(IfNumber, IfEntrysRem2),
%     {_IfMtu,     IfEntrysRem4}  = lists:split(IfNumber, IfEntrysRem3),
%     {_IfSpeed,   _IfPhysAddress} = lists:split(IfNumber, IfEntrysRem4),
%     generate_if_record(IfIndexes, IfEntrysRem1).
%     %?LOG({IfIndexes, IfDescr, IfType, IfMtu, IfSpeed, IfPhysAddress}).
% 
% generate_if_record(IfIndexes, IfEntrys) ->
%     generate_if_record([], IfIndexes, IfEntrys).
% generate_if_record(Result, [], _) ->
%     Result;
% generate_if_record(Result, [IfIndex | IfIndexes], IfEntrys) ->
%     {_,_,_,Index,  _}  = IfIndex,
%     {_,_,_,IfDescr,_} = lists:keyfind([1,3,6,1,2,1,2,2,1,2,Index], 2, IfEntrys),
%     {_,_,_,IfType, _} = lists:keyfind([1,3,6,1,2,1,2,2,1,3,Index], 2, IfEntrys),
%     {_,_,_,IfMtu,  _} = lists:keyfind([1,3,6,1,2,1,2,2,1,4,Index], 2, IfEntrys),
%     {_,_,_,IfSpeed,_} = lists:keyfind([1,3,6,1,2,1,2,2,1,5,Index], 2, IfEntrys),
%     {_,_,_,IfPhysAddress,_} = lists:keyfind([1,3,6,1,2,1,2,2,1,6,Index], 2, IfEntrys),
% 
%     If = #interface{
%         if_index        = Index,
%         if_descr        = IfDescr,
%         if_type         = IfType,
%         if_mtu          = IfMtu,
%         if_speed        = IfSpeed,
%         if_phys_address = IfPhysAddress
%     },
%     generate_if_record([If | Result], IfIndexes, IfEntrys).

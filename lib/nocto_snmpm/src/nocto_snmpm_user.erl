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
            };
        _   ->
            error_logger:info_report(
                "~p ~p: get_mib2_system received: ~p", [?MODULE, ?LINE, Reply]
            ),
            []
    end.

get_mib2_interfaces(Agent) ->
    IfIndexes       = sync_walk_bulk(Agent, ?OID_IF_INDEX),
    IfDescr         = sync_walk_bulk(Agent, ?OID_IF_DESCR),
    IfType          = sync_walk_bulk(Agent, ?OID_IF_TYPE),
    IfMTU           = sync_walk_bulk(Agent, ?OID_IF_MTU),
    IfSpeed         = sync_walk_bulk(Agent, ?OID_IF_SPEED),
    IfPhysAdd       = sync_walk_bulk(Agent, ?OID_IF_PHYS_ADDRESS),
    IfAdminStatus   = sync_walk_bulk(Agent, ?OID_IF_ADMIN_STATUS),
    IfOperStatus    = sync_walk_bulk(Agent, ?OID_IF_OPER_STATUS),
    IfLastChange    = sync_walk_bulk(Agent, ?OID_IF_LAST_CHANGE),
    Infos = [IfIndexes,IfDescr,IfType,IfMTU,IfSpeed,IfPhysAdd,IfAdminStatus,
        IfOperStatus, IfLastChange],

    Interfaces = generate_if_records(Infos),
    ?LOG2(Interfaces),
    ok.

generate_if_records([IfIndexes|Values]) ->
    Indexes = [Index || {varbind,_,_,Index,_} <- IfIndexes],
    generate_if_records(Indexes, Values, []).

generate_if_records([],_,Accum) -> Accum;
generate_if_records([Index|OtherIndexes], Values, Accum) ->

    [IfDescr,IfType,IfMTU,IfSpeed,IfPhysAdd,
        IfAdminStatus,IfOperStatus,IfLastChange] = Values,

    MatchIfDescr    = lists:append(?OID_IF_DESCR, [Index]),
    IfDescrTuple    = lists:keyfind(MatchIfDescr, 2, IfDescr),
    {_,_,_,Descr,_} = IfDescrTuple,

    MatchIfType     = lists:append(?OID_IF_TYPE, [Index]),
    IfTypeTuple     = lists:keyfind(MatchIfType, 2, IfType),
    {_,_,_,Type,_}  = IfTypeTuple,

    MatchIfMTU      = lists:append(?OID_IF_MTU, [Index]),
    IfMTUTuple      = lists:keyfind(MatchIfMTU, 2, IfMTU),
    {_,_,_,MTU,_}   = IfMTUTuple,

    MatchIfSpeed    = lists:append(?OID_IF_SPEED, [Index]),
    IfSpeedTuple    = lists:keyfind(MatchIfSpeed, 2, IfSpeed),
    {_,_,_,Speed,_} = IfSpeedTuple,

    MatchIfPhysAdd      = lists:append(?OID_IF_PHYS_ADDRESS, [Index]),
    IfPhysAddTuple      = lists:keyfind(MatchIfPhysAdd, 2, IfPhysAdd),
    {_,_,_,PhysAdd,_}   = IfPhysAddTuple,
    
    MatchIfAdminStatus  = lists:append(?OID_IF_ADMIN_STATUS, [Index]),
    IfAdminStatusTuple  = lists:keyfind(MatchIfAdminStatus, 2, IfAdminStatus),
    {_,_,_,AdminStatus,_}   = IfAdminStatusTuple,

    MatchIfOperStatus   = lists:append(?OID_IF_OPER_STATUS, [Index]),
    IfOperStatusTuple   = lists:keyfind(MatchIfOperStatus, 2, IfOperStatus),
    {_,_,_,OperStatus,_}   = IfOperStatusTuple,

    MatchIfLastChange   = lists:append(?OID_IF_LAST_CHANGE, [Index]),
    IfLastChangeTuple   = lists:keyfind(MatchIfLastChange, 2, IfLastChange),
    {_,_,_,LastChange,_}   = IfLastChangeTuple,

    Record = #mib2_interface{
        index           = Index,
        descr           = Descr,
        type            = Type,
        mtu             = MTU,
        speed           = Speed,
        phys_add        = PhysAdd,
        admin_status    = AdminStatus,
        oper_status     = OperStatus,
        last_change     = LastChange
    },
    generate_if_records(OtherIndexes,Values,[Record|Accum]).
    

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
                    FilteredR = remove_out_of_tree_OIDs(StartOID, R),
                    lists:append([Result, FilteredR])
            end;

        _ ->
            error_logger:info_report(
                "~p ~p: sync_walk_bulk received: ~p", [?MODULE, ?LINE, Reply]
            ),
            []
    end.

remove_out_of_tree_OIDs(Oid, List) ->
    lists:filter(fun({_,X,_,_,_}) ->
        still_in_tree(Oid, X)
    end, List).

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

-module(locator).
-behaviour(gen_server).
-include("include/locator.hrl").
-include("../nocto_snmpm/include/nocto_snmpm.hrl").
-define(SNMP_USER, "nocto_snmpm_user").

-define(SYS_NAME_OID,       [1,3,6,1,2,1,1,5,0]).
-define(SYS_SERVICES_OID,   [1,3,6,1,2,1,1,7,0]).
-define(IF_NUMBER_OID,      [1,3,6,1,2,1,2,1,0]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    start_link/0
]).

% -record(interface, {
%     if_index            = 0             :: integer(),
%     if_descr            = ""            :: string(),
%     if_type             = 0             :: integer(),
%     if_mtu              = 0             :: integer(),
%     if_speed            = 0             :: integer(),
%     if_phys_address     = []            :: [integer()]
% }).
% 
% -record(agent, {
%     name                = ""            :: string(),
%     sys_name            = ""            :: string(),
%     sys_services        = 0             :: integer(),
%     if_number           = 0             :: integer(),
%     interfaces          = []            :: [#interface{}]
% }).
% 
% -record(state, {
%     agents              = []            :: [#agent{}]
% }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Agents = nocto_snmpm_user:which_agents(),
    ?LOG(Agents),
    SysInfos = [nocto_snmpm_user:get_mib2_system(Agent) || Agent <- Agents],
    IfInfos  = [nocto_snmpm_user:get_mib2_interfaces(Agent) || Agent <- Agents],
    ?LOG(SysInfos),
    ?LOG(IfInfos),
    %Agents = [initialize_agent(Agent) || 
    %    Agent <- snmpm:which_agents(?SNMP_USER)],
    %{ok, #state{agents = Agents}}.
    {ok, []}.
%  
handle_call(_R, _F, S) ->
    {noreply, S}.

% 
handle_cast(_,S) ->
    {noreply, S}.

%
handle_info(_, S) ->
    {noreply, S}.

%
terminate(_,_) ->
    ok.

%
code_change(_,S,_) ->
    {ok, S}.

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

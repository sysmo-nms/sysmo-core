-module(locator_query).
-behaviour(gen_server).
-include("include/locator.hrl").
-include("../snmp_manager/include/snmp_manager.hrl").

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    start_link/1,
    random_switch_table_fetch/1,
    exec_switch_table_fetch/1,
    random_router_table_fetch/1,
    exec_router_table_fetch/1
]).

-record(state, {
    agent_name,
    switch_fetch_step,
    router_fetch_step
}).



% @doc
% Exported functions are only here for timer:apply function. These fuctions
% should not be called manualy.
% @end


% API
start_link(Cfg) ->
    gen_server:start_link(?MODULE, Cfg, []).

exec_switch_table_fetch(Pid) ->
    gen_server:cast(Pid, exec_switch_table_fetch).

random_switch_table_fetch(Pid) ->
    gen_server:cast(Pid, random_switch_table_fetch).

exec_router_table_fetch(Pid) ->
    gen_server:cast(Pid, exec_router_table_fetch).

random_router_table_fetch(Pid) ->
    gen_server:cast(Pid, random_router_table_fetch).


% GEN_SERVER BEHAVIOUR
init(#locator_agent{
        agent_name = AgentName,
        sys_infos  = #mib2_system{
            sys_services = SysServices
        }
    } = _LocatorAgent) ->

    case SysServices of
        #services{internet = true, datalink = true} ->
            ?LOG("switch and router"),
            random_router_table_fetch(self()),
            random_switch_table_fetch(self());
        #services{internet = true} ->
            random_router_table_fetch(self()),
            ?LOG("router");
        #services{datalink = true} ->
            ?LOG("switch"),
            random_switch_table_fetch(self())
    end,

    % FOR TEST ONLY random_router_table_fetch/1
    random_router_table_fetch(self()),
    % END

    _AgingTime = snmp_manager:get_dot1q_aging(AgentName),


    {ok, 
        #state{
            agent_name          = AgentName,
            switch_fetch_step   = 5,
            %switch_fetch_step  = AgingTime - 10 
            router_fetch_step   = 5
        }
    }.



% CALL
handle_call(R, _F, S) ->
    error_logger:info_msg(
        "~p ~p: handle_call received: ~p", [?MODULE, ?LINE, R]),
    {noreply, S}.



% CAST
% SWITCH TABLE FETCH
handle_cast(random_switch_table_fetch, 
        #state{switch_fetch_step = T} = S) ->
 
    random:seed(erlang:now()),
    Rand = random:uniform(T),

    timer:apply_after(Rand * 1000, ?MODULE, exec_switch_table_fetch, [self()]),
    {noreply, S};

handle_cast(exec_switch_table_fetch, 
        #state{agent_name = AgentName, switch_fetch_step = T} = S) ->

    Reply = switch_table_fetch(AgentName),
    locator:update_forward_infos(AgentName, Reply),

    timer:apply_after(T * 1000, ?MODULE, exec_switch_table_fetch, [self()]),
    {noreply, S};

% ROUTER TABLE FETCH
handle_cast(random_router_table_fetch, 
        #state{router_fetch_step = T} = S) ->
 
    random:seed(erlang:now()),
    Rand = random:uniform(T),

    timer:apply_after(Rand * 1000, ?MODULE, exec_router_table_fetch, [self()]),
    {noreply, S};

handle_cast(exec_router_table_fetch, 
        #state{agent_name = AgentName, router_fetch_step = T} = S) ->

    Reply = router_table_fetch(AgentName),
    locator:update_arp_infos(AgentName, Reply),

    timer:apply_after(T * 1000, ?MODULE, exec_router_table_fetch, [self()]),
    {noreply, S};



handle_cast(R,S) ->
    error_logger:info_msg(
        "~p ~p: handle_cast received: ~p", [?MODULE, ?LINE, R]),
    {noreply, S}.



% INFO
handle_info(R, S) ->
    error_logger:info_msg(
        "~p ~p: handle_info received: ~p", [?MODULE, ?LINE, R]),
    {noreply, S}.



% TERMINATE
terminate(R,_) ->
    error_logger:info_msg(
        "~p ~p: terminate: ~p", [?MODULE, ?LINE, R]),
    ok.



% CHANGE
code_change(_,S,_) ->
    {ok, S}.



% PRIVATE

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SWITCH FORWARD TABLE FETCH %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get dot1qTpFdbTable
switch_table_fetch(Agent) ->
    Reply = snmp_manager:get_dot1q_tpfdb_table(Agent),
    sw_format(Reply).

% dot1qTpFdbTable to [#dot1q_tpfdb_entry{}]
sw_format(Reply) ->
    sw_format(Reply, []).

sw_format([], MacPorts) ->
    MacPorts;

sw_format(
        [{_,[1,3,6,1,2,1,17,7,1,2,2,1,2 | Rest],_,IfIndex,_} | Reply], 
        MacPorts
    ) ->
    [Vlan | Mac] = Rest,
    Entry = #dot1q_tpfdb_entry{
        if_index = IfIndex,
        vlan = Vlan,
        mac_address = Mac
    },
    sw_format(Reply, [Entry | MacPorts]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ROUTER ARP TABLE FETCH %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get ipNetToMediaTable
router_table_fetch(Agent) ->
    Reply = snmp_manager:get_ipNetToMedia_table(Agent),
    rt_format(Reply).

rt_format(Reply) ->
    R   = rt_filter(Reply),
    rt_format([], R).

rt_format(Records, {[],_,_}) ->
    Records;
rt_format(Records, {[If|Ifs], Phys, Net}) ->
    {_, [1,3,6,1,2,1,4,22,1,1 | Rest], _, IfIndex, _} = If,

    PhysOid = [1,3,6,1,2,1,4,22,1,2 | Rest],
    NetOid  = [1,3,6,1,2,1,4,22,1,3 | Rest],

    {value, PhysFound, Phys2} = lists:keytake(PhysOid, 2, Phys),
    {value, NetFound, Net2}   = lists:keytake(NetOid,  2, Net),

    {_,_,_, PhysicalAddress,_}  = PhysFound,
    {_,_,_, NetworkAddress, _}  = NetFound,

    Record = #inet_to_media_entry{
        inet        = NetworkAddress,
        mac         = PhysicalAddress,
        if_index    = IfIndex
    },
    rt_format([Record | Records], {Ifs, Phys2, Net2}).

    

rt_filter(Reply) ->
    rt_filter({[],[],[]}, Reply).
rt_filter(Result, []) ->
    Result;
rt_filter({IfIndex, PhysAdd, NetAdd}, [R|Reply]) ->
    {_,Oid,_,_,_} = R,
    case Oid of
        [1,3,6,1,2,1,4,22,1,1|_] ->
            rt_filter({[R|IfIndex], PhysAdd, NetAdd}, Reply);
        [1,3,6,1,2,1,4,22,1,2|_] ->
            rt_filter({IfIndex, [R|PhysAdd], NetAdd}, Reply);
        [1,3,6,1,2,1,4,22,1,3|_] ->
            rt_filter({IfIndex, PhysAdd, [R|NetAdd]}, Reply);
        _ ->
            rt_filter({IfIndex, PhysAdd, NetAdd}, Reply)
    end.

% get ipNetToPhysicalTable NOT SUPPORTED BY ALCATEL AND ALLIED AT LEAST
% router_table_fetch(Agent) ->
%     Reply = snmp_manager:get_ipNetToPhysical_table(Agent),
%     rt_format(Reply).
% 
% % ipNetToPhysicalTable to [#inet_to_physical_entry{}]
% rt_format(Reply) ->
%     R   = rt_filter(Reply),
%     Rep = rt_format(R, []),
%     ?LOG(Rep).
% 
% rt_format({[], _}, Result) ->
%     Result;
% rt_format({[Amac|Tail], Tt}, Result) ->
%     {_,Oid,_,Mac,_} = Amac,
%     [1,3,6,1,2,1,4,35,1,4 | Infos] = Oid,
%     {value, Found, Tt2} = lists:keytake([1,3,6,1,2,1,4,35,1,5 | Infos], 2, Tt),
%     ?LOG({Found, Mac}),
%     rt_format({Tail, Tt2}, Result).
% 
% 
% 
% 
% 
% rt_filter(Reply) ->
%     rt_filter({[], []}, Reply).
% 
% rt_filter(Filtered, []) ->
%     Filtered;
% rt_filter(
%         {Mac, TimeTicks}, 
%         [{_,[1,3,6,1,2,1,4,35,1,4|_],_,_,_} = Mc | Reply] ) ->
%     rt_filter({[Mc|Mac], TimeTicks}, Reply);
% rt_filter(
%         {Mac, TimeTicks}, 
%         [{_,[1,3,6,1,2,1,4,35,1,5|_],_,_,_} = Tt | Reply] ) ->
%     rt_filter({Mac, [Tt|TimeTicks]}, Reply);
% rt_filter(Filtered, [_|Reply]) ->
%     rt_filter(Filtered,Reply).
% 

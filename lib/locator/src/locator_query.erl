-module(locator_query).
-behaviour(gen_server).
-include("include/locator.hrl").
-include("../nocto_snmpm/include/nocto_snmpm.hrl").

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
    exec_switch_table_fetch/1
]).

-record(state, {
    agent_name,
    arp_fetch_step
}).




% API
start_link(Cfg) ->
    gen_server:start_link(?MODULE, Cfg, []).

exec_switch_table_fetch(Pid) ->
    gen_server:cast(Pid, exec_switch_table_fetch).




% GEN_SERVER BEHAVIOUR
init(#locator_agent{
        agent_name = AgentName,
        sys_infos  = #mib2_system{
            sys_services = SysServices
        }
    } = _LocatorAgent) ->

    case SysServices of
        #services{internet = true, datalink = true} ->
            ?LOG("switch and router");
        #services{internet = true} ->
            ?LOG("router");
        #services{datalink = true} ->
            ?LOG("switch")
    end,

    _AgingTime       = nocto_snmpm_user:get_dot1q_aging(AgentName),
    gen_server:cast(self(), initial_switch_table_fetch),
    {ok, 
        #state{
            agent_name      = AgentName,
            arp_fetch_step  = 5
            %arp_fetch_step  = AgingTime - 10 
        }
    }.



% CALL
handle_call(R, _F, S) ->
    error_logger:info_msg(
        "~p ~p: handle_call received: ~p", [?MODULE, ?LINE, R]),
    {noreply, S}.



% CAST
handle_cast(initial_switch_table_fetch, 
        #state{arp_fetch_step = T} = S) ->
    random:seed(erlang:now()),
    Rand = random:uniform(T),
    timer:apply_after(Rand * 1000, ?MODULE, exec_switch_table_fetch, [self()]),
    {noreply, S};

handle_cast(exec_switch_table_fetch, 
        #state{agent_name = A, arp_fetch_step = T} = S) ->
    ?LOG(A),
    Rep = switch_table_fetch(A),
    locator:update(A, Rep),
    timer:apply_after(T * 1000, ?MODULE, exec_switch_table_fetch, [self()]),
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
switch_table_fetch(Agent) ->
    % TODO format datas.
    ?LOG('table fetch'),
    [H|Rep] = nocto_snmpm_user:get_dot1q_tpfdb_table(Agent),
    ?LOG(H),
    Rep.

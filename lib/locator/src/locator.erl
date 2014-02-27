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
    start_link/0,
    update/2
]).

-record(state, {
    agents
}).

% API
update(Agent, Values) ->
    gen_server:cast(?MODULE, {update, Agent, Values}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



% GEN_SERVER
init([]) ->
    Agents = nocto_snmpm_user:which_agents(),
    AgentsRecords = [
        #locator_agent{
            agent_name  = Agent,
            sys_infos   = nocto_snmpm_user:get_mib2_system(Agent),
            if_infos    = nocto_snmpm_user:get_mib2_interfaces(Agent)
        } || Agent <- Agents],

    ?LOG(AgentsRecords),
    ok = initialize_queries(AgentsRecords),
    {ok, #state{agents = AgentsRecords}}.



% CALL 
handle_call(_R, _F, S) ->
    {noreply, S}.



% CAST
handle_cast({update, _Agent, _Values},S) ->
    % TODO update datas (in memory mnesia)
    ?LOG('update!'),
    {noreply, S};

handle_cast(_,S) ->
    {noreply, S}.



% INFO
handle_info(_, S) ->
    {noreply, S}.



% TERMINATE
terminate(_,_) ->
    ok.


% CHANGE
code_change(_,S,_) ->
    {ok, S}.


% private
initialize_queries(Agents) ->
    lists:foreach(fun(X) ->
        locator_query_sup:launch(X)
    end, Agents).

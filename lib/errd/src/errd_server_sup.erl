%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @doc errd_server supervisor
%% @end
%%%-------------------------------------------------------------------
-module(errd_server_sup).

-behaviour(supervisor).

%% API
-export([
    start_link/0,
    create_instance/0
]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc: Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec create_instance() -> {ok, pid()} | ignore | {error, any()}.
create_instance() ->
    supervisor:start_child(?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init
%% @spec (Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                 ignore                          |
%%                 {error, Reason}
%% @doc Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok,
        {
            {simple_one_for_one, 1, 2},
            [
                {
                    errd_server,
                    {errd_server,start_link,[]},
                    permanent,
                    2000,
                    worker,
                    [errd_server, errd_command]
                }
            ]
        }
    }.

%%====================================================================
%% Internal functions
%%====================================================================

% vim: set ts=4 sw=4 expandtab:

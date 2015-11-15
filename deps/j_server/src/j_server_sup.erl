% @private
-module(j_server_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok,
        {
            {one_for_all, 0, 6000},
            [
                {
                    j_server,
                    {j_server, start_link, []},
                    permanent,
                    15000,
                    worker,
                    [j_server]
                },
                {
                    j_server_nchecks,
                    {j_server_nchecks, start_link, []},
                    permanent,
                    2000,
                    worker,
                    [j_server_nchecks]
                },
                {
                    j_server_errd4j,
                    {j_server_errd4j, start_link, []},
                    permanent,
                    2000,
                    worker,
                    [j_server_errd4j]
                },
                {
                    j_server_snmpman,
                    {j_server_snmpman, start_link, []},
                    permanent,
                    2000,
                    worker,
                    [j_server_snmpman]
                },
                {
                    j_server_eventdb,
                    {j_server_eventdb, start_link, []},
                    permanent,
                    2000,
                    worker,
                    [j_server_eventdb]
                }
            ]
        }
    }.

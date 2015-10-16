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
            {one_for_all, 1, 6000},
            [
                {
                    j_server,
                    {j_server, start_link, []},
                    permanent,
                    2000,
                    worker,
                    [j_server]
                },
                {
                    nchecks,
                    {nchecks, start_link, []},
                    permanent,
                    2000,
                    worker,
                    [nchecks]
                },
                {
                    errd4j,
                    {errd4j, start_link, []},
                    permanent,
                    2000,
                    worker,
                    [errd4j]
                },
                {
                    snmpman,
                    {snmpman, start_link, []},
                    permanent,
                    2000,
                    worker,
                    [snmpman]
                },
                {
                    eventdb,
                    {eventdb, start_link, []},
                    permanent,
                    2000,
                    worker,
                    [eventdb]
                }
            ]
        }
    }.

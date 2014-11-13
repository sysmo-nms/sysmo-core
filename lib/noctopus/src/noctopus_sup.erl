% @private
-module(noctopus_sup).
-behaviour(supervisor).

-export([
    start_link/0
]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, 
        {
            {one_for_all, 1, 6000},
            [
                {
                    snmpman_app,
                    {snmpman_app, start, [normal,[]]},
                    permanent,
                    2000,
                    supervisor,
                    [snmpman_app]
                },
                {
                    errd_app,
                    {errd_app, start, [normal,[]]},
                    permanent,
                    2000,
                    supervisor,
                    [errd_app]
                },
                {
                    nchecks_app,
                    {nchecks_app, start, [normal,[]]},
                    permanent,
                    2000,
                    supervisor,
                    [nchecks_app]
                },
                {
                    equartz_app,
                    {equartz_app, start, [normal,[]]},
                    permanent,
                    2000,
                    supervisor,
                    [equartz_app]
                },

                {
                    monitor_app,
                    {monitor_app, start, [normal,[]]},
                    permanent,
                    2000,
                    supervisor,
                    [monitor_app]
                },
                {
                    supercast_app,
                    {supercast_app, start, [normal,[]]},
                    permanent,
                    2000,
                    supervisor,
                    [supercast_app]
                }
            ]
        }
    }.

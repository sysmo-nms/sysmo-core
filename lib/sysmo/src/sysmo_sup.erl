% @private
-module(sysmo_sup).
-behaviour(supervisor).

-export([
    start_link/0
]).
-export([init/1]).

start_link() ->
    case mnesia:system_info(use_dir) of
        false -> mnesia:create_schema([node()]);
        true  -> ok
    end,
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, 
        {
            {one_for_all, 1, 6000},
            [

                {
                    mnesia_sup,
                    {mnesia_sup, start, [normal,[]]},
                    permanent,
                    2000,
                    supervisor,
                    [mnesia_sup]
                },
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
                    supercast_app,
                    {supercast_app, start, [normal,[]]},
                    permanent,
                    2000,
                    supervisor,
                    [supercast_app]
                },
                {
                    monitor_app,
                    {monitor_app, start, [normal,[]]},
                    permanent,
                    2000,
                    supervisor,
                    [monitor_app]
                }
            ]
        }
    }.

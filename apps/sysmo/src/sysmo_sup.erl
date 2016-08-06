%%=
%%=
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
                    j_server_app,
                    {j_server_app, start, [normal,[]]},
                    permanent,
                    2000,
                    supervisor,
                    [j_server_app]
                },
                {
                    supercast_app,
                    {supercast_app, start, [normal,[]]},
                    permanent,
                    2000,
                    supervisor,
                    [supercast_app]
                },
                { %% only important is monitor last
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

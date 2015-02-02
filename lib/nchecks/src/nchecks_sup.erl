% @private
-module(nchecks_sup).
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
            {one_for_one, 10, 60},
            [
                {
                    nchecks,
                    {nchecks, start_link, []},
                    permanent,
                    2000,
                    worker,
                    [nchecks]
                }
            ]
        }
    }.

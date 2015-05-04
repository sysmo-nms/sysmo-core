% @private
-module(errd4j_sup).
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
                    errd4j,
                    {errd4j, start_link, []},
                    permanent,
                    2000,
                    worker,
                    [errd4j]
                }
            ]
        }
    }.

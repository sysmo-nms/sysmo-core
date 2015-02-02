% @private
-module(errd_sup).
-behaviour(supervisor).
-include("include/errd.hrl").

-export([
    start_link/0
]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, RrdNiceHigh}   = application:get_env(errd, high_prio),
    {ok, RrdNiceLow}    = application:get_env(errd, low_prio),
    {ok, RrdTimeoutHigh}= application:get_env(errd, high_timeout),
    {ok, RrdTimeoutLow} = application:get_env(errd, low_timeout),
    {ok, 
        {
            {one_for_one, 10, 60},
            [
                {
                    ?HIGH_PRIO_SRV,
                    {
                        errd_server,
                        start_link,
                        [?HIGH_PRIO_SRV, RrdNiceHigh, RrdTimeoutHigh]
                    },
                    permanent,
                    2000,
                    worker,
                    [errd_server]
                },
                {
                    ?LOW_PRIO_SRV,
                    {
                        errd_server,
                        start_link,
                        [?LOW_PRIO_SRV, RrdNiceLow, RrdTimeoutLow]
                    },
                    permanent,
                    2000,
                    worker,
                    [errd_server]
                },
                {
                    ?HIGH_PRIO_CALL_QUEUE,
                    {
                        errd_server_call_queue,
                        start_link,
                        [?HIGH_PRIO_CALL_QUEUE, ?HIGH_PRIO_SRV]
                    },
                    permanent,
                    2000,
                    worker,
                    [errd_server_call_queue]
                },
                {
                    ?LOW_PRIO_CALL_QUEUE,
                    {
                        errd_server_call_queue,
                        start_link,
                        [?LOW_PRIO_CALL_QUEUE, ?LOW_PRIO_SRV]
                    },
                    permanent,
                    2000,
                    worker,
                    [errd_server_call_queue]
                }
            ]
        }
    }.

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
                    errdtools_app,
                    {errdtools_app, start, [normal,[]]},
                    permanent,
                    2000,
                    supervisor,
                    [errdtools_app]
                },
                {
                    text_logger_app,
                    {text_logger_app, start, [normal,[]]},
                    permanent,
                    2000,
                    supervisor,
                    [text_logger_app]
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

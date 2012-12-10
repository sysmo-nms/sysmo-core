-module(template_events).
-export([start_link/0]).

start_link() ->
    % START the event manager:
    {ok, _} = Return = gen_event:start_link({local, ?MODULE}),
    % add a default handler for debug:
    gen_event:add_handler(?MODULE, template_terminal_logger, []),
    % return to the supervisor
    Return.

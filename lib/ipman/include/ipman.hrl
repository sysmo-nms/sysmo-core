-define(LOG(X), io:format("{~p, ~p}: DEBUG: ~p~n", [?MODULE, ?LINE, X])).

-record(ipman_agent, {
    agent_name,
    sys_infos,
    if_infos
}).

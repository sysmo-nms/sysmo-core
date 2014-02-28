-define(LOG(X), io:format("{~p, ~p}: DEBUG: ~p~n", [?MODULE, ?LINE, X])).

-record(ipman_agent, {
    agent_name,
    sys_infos,
    net_infos
}).

-record(ip_addr_entry, {
    ip,
    mask,
    if_index
}).

#!/usr/bin/env escript

main(_) ->
    {ok, Hostname} = inet:gethostname(),
    SrvNode  = list_to_atom(
        lists:flatten(io_lib:format("master@~s", [Hostname]))
    ),
    R = net_kernel:connect_node(SrvNode),
    io:format("hello ~p ~p ~p~n", [node(), R, SrvNode]),
    receive 
        _ -> ok
    end.

#!/usr/bin/env escript

main(_) ->
    io:format("hello ~p~n", [node()]),
    receive 
        _ -> ok
    end.

-module(sysmo).
-export([
    get_java_dir/0,
    get_java_bin_prefix/0,
    get_node_name/0

]).

get_java_dir() ->
    {ok, Dir} = application:get_env(sysmo, java_dir),
    Dir.

get_java_bin_prefix() ->
    case os:type() of
        {win32,_} -> ".bat";
        {_,_} -> ""
    end.

get_node_name() -> erlang:atom_to_list(erlang:node()).

-module(sysmo).
-include_lib("common_hrl/include/logs.hrl").

-behaviour(gen_server).

% gen_server
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,
         terminate/2,code_change/3]).

% utils
-export([start_link/0]).

% other
-export([
    get_java_dir/0,
    get_java_bin_prefix/0,
    get_node_name/0
]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Prefix   = get_java_bin_prefix(),
    Relative = string:concat("sysmo-jserver/bin/sysmo-jserver", Prefix),
    Cmd      = filename:join(
                 filename:absname(get_java_dir()), Relative),
    WorkDir  = filename:absname(""),
    Node     = get_node_name(),
    Cookie   = get_cookie_string(),
    Port     = erlang:open_port({spawn_executable, Cmd},
                [{args,[Node, Cookie, WorkDir]},
                 {cd, WorkDir},
                 exit_status,
                 stderr_to_stdout]),
    {ok, Port}.

handle_call(Call, _, S) ->
    ?LOG_WARNING("Received unknow call", Call),
    {noreply, S}.

handle_cast(Cast, S) ->
    ?LOG_WARNING("Received unknow cast", Cast),
    {noreply, S}.

handle_info({_Port, {exit_status, Status}}, S) ->
    ?LOG_ERROR("java node has crashed", {status, Status}),
    {stop, "Java node has crashed", S};

handle_info(Info, S) ->
    ?LOG_WARNING("Received unknow info", Info),
    {noreply, S}.

terminate(_,_) ->
    ok.

code_change(_,S,_) ->
     {ok, S}.

get_java_dir() ->
    {ok, Dir} = application:get_env(sysmo, java_dir),
    Dir.

get_java_bin_prefix() ->
    case os:type() of
        {win32,_} -> ".bat";
        {_,_} -> ""
    end.

get_node_name() ->
    erlang:atom_to_list(erlang:node()).

get_cookie_string() ->
    erlang:atom_to_list(erlang:get_cookie()).

% @private
-module(sysmo_app).
-behaviour(application).
-include_lib("kernel/include/file.hrl").

-export([
    start/2,
    stop/1
]).

start(_Type, _Args) ->
    set_cookie(),
    sysmo_sup:start_link().

stop(_State) ->
    ok.

set_cookie()  ->
    {ok, Cookie} = read_cookie("etc/erlang.cookie"),
    erlang:set_cookie(node(), erlang:list_to_atom(Cookie)).

%% Taken from undocumented lib/kernel/src/auth.erl
read_cookie(Name) ->
    case file:raw_read_file_info(Name) of
        {ok, #file_info {type=Type, mode=Mode, size=Size}} ->
            case check_attributes(Name, Type, Mode, os:type()) of
                ok -> read_cookie(Name, Size);
                Error -> Error
            end;
        {error, enoent} ->
            case create_cookie(Name) of
                ok -> read_cookie(Name);
                Error -> Error
            end;
        {error, Reason} ->
            {error, make_error(Name, Reason)}
    end.

read_cookie(Name, Size) ->
    case file:open(Name, [raw, read]) of
        {ok, File} ->
            case file:read(File, Size) of
                {ok, List} ->
                    ok = file:close(File),
                    check_cookie(List, []);
                {error, Reason} ->
                    make_error(Name, Reason)
            end;
        {error, Reason} ->
            make_error(Name, Reason)
    end.

check_attributes(Name, Type, _Mode, _Os) when Type =/= regular ->
    {error, "Cookie file " ++ Name ++ " is of type " ++ Type};
check_attributes(Name, _Type, Mode, {unix, _}) when (Mode band 8#077) =/= 0 ->
    {error, "Cookie file " ++ Name ++ " must be accessible by owner only"};
check_attributes(_Name, _Type, _Mode, _Os) ->
    ok.

make_error(Name, Reason) ->
    {error, "Error when reading " ++ Name ++ ": " ++ atom_to_list(Reason)}.

check_cookie([Letter|Rest], Result) when $\s =< Letter, Letter =< $~ ->
    check_cookie(Rest, [Letter|Result]);
check_cookie([X|Rest], Result) ->
    check_cookie1([X|Rest], Result);
check_cookie([], Result) ->
    check_cookie1([], Result).

check_cookie1([$\n|Rest], Result) ->
    check_cookie1(Rest, Result);
check_cookie1([$\r|Rest], Result) ->
    check_cookie1(Rest, Result);
check_cookie1([$\s|Rest], Result) ->
    check_cookie1(Rest, Result);
check_cookie1([_|_], _Result) ->
    {error, "Bad characters in cookie"};
check_cookie1([], []) ->
    {error, "Too short cookie string"};
check_cookie1([], Result) ->
    {ok, lists:reverse(Result)}.

make_info(Name) ->
    Midnight =
        case file:raw_read_file_info(Name) of
            {ok, #file_info{atime={Date, _}}} ->
                {Date, {0, 0, 0}};
            _ ->
                {{1990, 1, 1}, {0, 0, 0}}
        end,
    #file_info{mode=8#400, atime=Midnight, mtime=Midnight, ctime=Midnight}.

create_cookie(Name) ->
    {_, S1, S2} = now(),
    Seed = S2*10000+S1,
    Cookie = random_cookie(20, Seed, []),
    case file:open(Name, [write, raw]) of
        {ok, File} ->
            R1 = file:write(File, Cookie),
            ok = file:close(File),
            R2 = file:raw_write_file_info(Name, make_info(Name)),
            case {R1, R2} of
                {ok, ok} ->
                    ok;
                {{error,Reason}, _} ->
                    {error,
                        lists:flatten(
                            io_lib:format("Failed to write to cookie file '~ts': ~p", [Name, Reason]))};
                {ok, {error, Reason}} ->
                    {error, "Failed to change mode: " ++ atom_to_list(Reason)}
            end;
        {error,Reason} ->
            {error,
                lists:flatten(
                    io_lib:format("Failed to create cookie file '~ts': ~p", [Name, Reason]))}
    end.

random_cookie(0, _, Result) ->
    Result;
random_cookie(Count, X0, Result) ->
    X= next_random(X0),
    Letter = X*($Z-$A+1) div 16#1000000000 + $A,
    random_cookie(Count-1, X, [Letter|Result]).

next_random(X) ->
    (X*17059465+1) band 16#fffffffff.


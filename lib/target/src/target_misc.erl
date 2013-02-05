% This file is part of "Enms" (http://sourceforge.net/projects/enms/)
% Copyright (C) 2012 <Sébastien Serre sserre.bx@gmail.com>
% 
% Enms is a Network Management System aimed to manage and monitor SNMP
% target, monitor network hosts and services, provide a consistent
% documentation system and tools to help network professionals
% to have a wide perspective of the networks they manage.
% 
% Enms is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
% 
% Enms is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with Enms.  If not, see <http://www.gnu.org/licenses/>.
-module(target_misc).
-behaviour(gen_server).
-include_lib("../include/target.hrl").

-export([
    start_link/0,
    generate_id/0,
    fill_target_store/0,
    clear_target_store/0,
    some_ips/0,
    random/1
]).

-export([
    init/1,
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

%%-------------------------------------------------------------
%% without this small server utility, random:uniform is called
%% at the same time at startup and return identical values.
%%-------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

random(V) ->
    gen_server:call(?MODULE, {random, V}).

init([]) ->
    random:seed(),
    {ok, state}.
    
handle_call({random, V}, _F, S) ->
    {reply, random:uniform(V), S};
handle_call(_R, _F, S) ->
    {noreply, S}.

handle_cast(_R, S) ->
    {noreply, S}.

handle_info(_I, S) ->
    {noreply, S}.

terminate(_R, _S) ->
    normal.

code_change(_O, S, _E) ->
    {ok, S}.
%%-------------------------------------------------------------
%% end of gen_server
%%-------------------------------------------------------------








-spec generate_id() -> string().
% @doc generate_id. genere un ash pour le navigateur client
%		tips: pour etre sur de retourner une valeur unique on
%		peut utiliser le #user record + le erlang:universaltime.
%		L'existance du resultat dans mnesia est verifié. Si il
%		existe, il est relancé.
%. .   .   binary()
% @end
generate_id() ->
	generate_id({erlang:universaltime(), random:uniform()}).

generate_id(Term) ->
	Bin = erlang:md5(erlang:term_to_binary(Term)),
	List = erlang:binary_to_list(Bin),
	HexList = lists:map(fun(X) -> dec_to_hex(X) end, List),
	FinalList = concat_id(HexList),
	Id = list_to_binary(FinalList),
	erlang:list_to_atom("target-" ++ erlang:binary_to_list(Id)).

-spec fill_target_store() -> ok | any().
% @doc
% fill target_store with empty target records.
% @end
fill_target_store() ->
    lists:foreach(fun(X) -> target_store:new(X) end, some_ips()).

clear_target_store() ->
    lists:foreach(fun(X) ->
        target_store:del_target(X)
    end, target_store:get_ids()).




% @private
%%--------------------------------------------------------------
%% @doc fonctions pour formater le nouveau id
%% @end
%%--------------------------------------------------------------
dec_to_hex(Dec) ->
	Rem = Dec rem 16,
	Div = Dec div 16,
	dec_to_hex(Div, [map_dec_to_hex(Rem)]).

dec_to_hex(0, List) ->
	List;

dec_to_hex(Dec, List) ->
	Rem = Dec rem 16,
	Div = Dec div 16,
	dec_to_hex(Div, [map_dec_to_hex(Rem) | List]).

map_dec_to_hex(Dec) ->
	case Dec of
		0 -> $0;
		1 -> $1;
		2 -> $2;
		3 -> $3;
		4 -> $4;
		5 -> $5;
		6 -> $6;
		7 -> $7;
		8 -> $8;
		9 -> $9;
		10 -> $A;
		11 -> $B;
		12 -> $C;
		13 -> $D;
		14 -> $E;
		15 -> $F
	end.

concat_id([H | T]) ->
	concat_id(T, H).

concat_id([], Final) ->
	Final;

concat_id([A | B], Final) ->
	concat_id(B, A ++ Final).


some_ips() ->
    [
        {173,194,34,63},
        {173,194,34,56},
        {173,194,34,55},
        {212,27,48,10},
        {77,238,178,122},
        {77,238,178,122},
        {87,248,120,148}
    ].

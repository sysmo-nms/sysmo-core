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
% @private
-module(tracker_misc).
-behaviour(gen_server).
-include("../include/tracker.hrl").

-export([
    start_link/0,
    generate_id/0,
    fill_target_store/0,
    fill_target_more/0,
    clear_target_store/0,
    some_ips/0,
    more_ips/0,
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
    lists:foreach(fun(X) -> tracker_target_store:new(X) end, some_ips()).

fill_target_more() ->
    lists:foreach(fun(X) -> tracker_target_store:new(X) end, more_ips()).

clear_target_store() ->
    lists:foreach(fun(X) ->
        tracker_target_store:del_target(X)
    end, tracker_target_store:get_ids()).




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
        {192,168,1,2},
        {173,194,34,63},
        {173,194,34,56},
        {173,194,34,55},
        {212,27,48,10},
        {77,238,178,122},
        {77,238,178,122},
        {87,248,120,148}
    ].

more_ips() ->
    [
        {192,168,1,2},
        {74,125,230,0},
        {74,125,230,1},
        {74,125,230,2},
        {74,125,230,3},
        {74,125,230,4},
        {74,125,230,5},
        {74,125,230,6},
        {74,125,230,7},
        {74,125,230,8},
        {74,125,230,9},
        {74,125,230,10},
        {74,125,230,11},
        {74,125,230,12},
        {74,125,230,13},
        {74,125,230,14},
        {74,125,230,15},
        {74,125,230,16},
        {74,125,230,17},
        {74,125,230,18},
        {74,125,230,19},
        {74,125,230,20},
        {74,125,230,21},
        {74,125,230,22},
        {74,125,230,23},
        {74,125,230,24},
        {74,125,230,25},
        {74,125,230,26},
        {74,125,230,27},
        {74,125,230,28},
        {74,125,230,29},
        {74,125,230,30},
        {74,125,230,31},
        {74,125,230,32},
        {74,125,230,33},
        {74,125,230,34},
        {74,125,230,35},
        {74,125,230,36},
        {74,125,230,37},
        {74,125,230,38},
        {74,125,230,39},
        {74,125,230,40},
        {74,125,230,41},
        {74,125,230,42},
        {74,125,230,43},
        {74,125,230,44},
        {74,125,230,45},
        {74,125,230,46},
        {74,125,230,47},
        {74,125,230,48},
        {74,125,230,49},
        {74,125,230,50},
        {74,125,230,51},
        {74,125,230,52},
        {74,125,230,53},
        {74,125,230,54},
        {74,125,230,55},
        {74,125,230,56},
        {74,125,230,57},
        {74,125,230,58},
        {74,125,230,59},
        {74,125,230,60},
        {74,125,230,61},
        {74,125,230,62},
        {74,125,230,63},
        {74,125,230,192},
        {74,125,230,193},
        {74,125,230,194},
        {74,125,230,195},
        {74,125,230,196},
        {74,125,230,197},
        {74,125,230,198},
        {74,125,230,199},
        {74,125,230,200},
        {74,125,230,201},
        {74,125,230,202},
        {74,125,230,203},
        {74,125,230,204},
        {74,125,230,205},
        {74,125,230,206},
        {74,125,230,207},
        {74,125,230,208},
        {74,125,230,209},
        {74,125,230,210},
        {74,125,230,211},
        {74,125,230,212},
        {74,125,230,213},
        {74,125,230,214},
        {74,125,230,215},
        {74,125,230,216},
        {74,125,230,217},
        {74,125,230,218},
        {74,125,230,219},
        {74,125,230,220},
        {74,125,230,221},
        {74,125,230,222},
        {74,125,230,223},
        {74,125,230,224},
        {74,125,230,225},
        {74,125,230,226},
        {74,125,230,227},
        {74,125,230,228},
        {74,125,230,229},
        {74,125,230,230},
        {74,125,230,231},
        {74,125,230,232},
        {74,125,230,233},
        {74,125,230,234},
        {74,125,230,235},
        {74,125,230,236},
        {74,125,230,237},
        {74,125,230,238},
        {74,125,230,239},
        {74,125,230,240},
        {74,125,230,241},
        {74,125,230,242},
        {74,125,230,243},
        {74,125,230,244},
        {74,125,230,245},
        {74,125,230,246},
        {74,125,230,247},
        {74,125,230,248},
        {74,125,230,249},
        {74,125,230,250},
        {74,125,230,251},
        {74,125,230,252},
        {74,125,230,253},
        {74,125,230,254},
        {74,125,230,255}
    ].
        

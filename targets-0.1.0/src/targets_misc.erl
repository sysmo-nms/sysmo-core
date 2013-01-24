% This file is part of "Enms" (http://sourceforge.net/projects/enms/)
% Copyright (C) 2012 <Sébastien Serre sserre.bx@gmail.com>
% 
% Enms is a Network Management System aimed to manage and monitor SNMP
% targets, monitor network hosts and services, provide a consistent
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
-module(targets_misc).
-export([
    generate_id/0
]).

%%--------------------------------------------------------------
%% @doc generate_id. genere un ash pour le navigateur client
%%		tips: pour etre sur de retourner une valeur unique on
%%		peut utiliser le #user record + le erlang:universaltime.
%%		L'existance du resultat dans mnesia est verifié. Si il
%%		existe, il est relancé.
%%. .   .   binary()
%% @end
%%--------------------------------------------------------------
generate_id() ->
	generate_id({erlang:universaltime(), random:uniform()}).

generate_id(Term) ->
	Bin = erlang:md5(erlang:term_to_binary(Term)),
	List = erlang:binary_to_list(Bin),
	HexList = lists:map(fun(X) -> dec_to_hex(X) end, List),
	FinalList = concat_id(HexList),
	Id = list_to_binary(FinalList),
	erlang:binary_to_list(Id).










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

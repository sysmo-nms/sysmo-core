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
    random/1,
    extract_nag_uom/1,
    timestamp/1
]).

-export([
    init/1,
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

-record(state, {
    nagios_uom_re
}).

%%-------------------------------------------------------------
%% without this small server utility, random:uniform is called
%% at the same time at startup and return identical values.
%% It also keep state the compiled re:compile version for the 
%% nagios compat module
%%-------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

random(V) ->
    gen_server:call(?MODULE, {random, V}).

extract_nag_uom(String) ->
    gen_server:call(?MODULE, {extract_nag_uom, String}).

init([]) ->
    random:seed(),
    NagUomRe = [
        {usecond,       "us$"   },
        {msecond,       "ms$"   },
        {second,        "s$"    },
        {percent,       "%$"    },
        {kbytes,        "KB$"   },
        {mbytes,        "MB$"   },
        {tbytes,        "TB$"   },
        {bytes,         "B$"    },
        {counter,       "c$"    }
    ],

    NagCompiledRe = lists:map(fun({Unit, Re}) ->
        {ok, RE} = re:compile(Re),
        {Unit, RE}
    end, NagUomRe),
    {ok, #state{nagios_uom_re = NagCompiledRe}}.
    

handle_call({extract_nag_uom, String}, _F, S) ->
    Rep = nag_uom_test(String, S#state.nagios_uom_re),
    {reply, {ok, Rep}, S};

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

timestamp(second) ->
    {Meg, Sec, _} = os:timestamp(),
    Meg * 1000000 + Sec;

timestamp(microsecond) ->
    {Meg, Sec, Mic} = os:timestamp(),
    Meg * 1000000 * 1000000 + Sec * 1000000 + Mic.

nag_uom_test(String, []) ->
    {String, no_unit};

nag_uom_test(String, [{ReName, RE} | ReList]) ->
    case re:run(String, RE) of
        nomatch     -> 
            nag_uom_test(String, ReList);
        {match, _}  ->
            [Val, _] = re:replace(String, RE, ""),
            {erlang:binary_to_list(Val), ReName}
    end.

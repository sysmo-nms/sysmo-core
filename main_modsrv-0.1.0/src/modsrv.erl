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
-module(modsrv).
-behaviour(gen_server).
-include_lib("../../include/eunit.hrl").
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% API
-export([start_link/0, hello/1, bye/1, get_modules/0]).



% @doc Start the server.
% Note that this module must be started before all others main modules.
% @end
-spec modsrv:start_link() -> {ok, pid()} | ignore | {error, Err::term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).




% @doc Call from a module when it start.
% <p>ArgList must be 1 or more values: <ul>
% <li> {modsrv_callback, ModuleName} where ModuleName must implement a
% callback function ModuleName:modsrv_notify/1 to receive the informations
% concerning the availables modules. MANDATORY </li>
% <li> {event_handler, EventHandlerMod} where EventHandlerMod is an atom
% registered as a gen_event handler if the module offer some events
% of interest for other modules, OPTIONAL</li>
% </ul>
% </p>
% <p>It will:
% <ul>
% <li> update the current available modules database </li>
% <li> notify actual and future modules of the availability
% of module ModName,</li> 
% <li> notify ModName of the actual available modules providing an 
% {event_callback, EventMod} argument.</li></ul></p>
% @end
-spec modsrv:hello(Arg :: {ModName :: atom(), ArgList :: list()}) -> ok.
hello({ModName, Opts}) ->
    {value, {_, CallBackMod}} = lists:keysearch(modsrv_callback, 1, Opts),
    gen_server:call(?MODULE, {hello, {ModName,Opts}, CallBackMod}).



% @doc Call from a module when he chutdown.
-spec modsrv:bye(ModName :: atom()) -> ok.
bye(Arg) ->
    gen_server:call(?MODULE, {bye, Arg}).

% @doc Get the entire module list.
-spec modsrv:get_modules() -> Modules::list().
get_modules() ->
    gen_server:call(?MODULE, dump).


% @private
init([]) ->
    {ok, []}.

% @private
handle_call({hello, {ModName, OptList} = Args, CallBackMod}, _From, State) ->
    % does the module ModName allready registered? If yes retire it.
    case lists:keysearch(ModName, 1, State) of
        {value, _ } ->
            CleanState = lists:filter(fun({Mod,_}) -> 
                if Mod == ModName -> false; true -> true end
            end, State);
        _           ->
            CleanState = State
    end,
    % new mod list is:
    NewState = [Args | CleanState],
    lists:foreach(fun({Mod, Opts}) -> 
        {value, {_, CallOtherMod}} = lists:keysearch(modsrv_callback, 1, Opts),
        % inform ModName of available modules
        CallBackMod:modsrv_notify({Mod, Opts}),
        % and inform other mods of the availability of ModName
        CallOtherMod:modsrv_notify({ModName, OptList})
    end, CleanState),
    {reply, ok, NewState};

handle_call({bye, Arg}, _From, State) ->
    % pop out the closing module
    NewState = lists:filter(fun(X) ->
        case X of
            {Arg, _}    -> false;
            _           -> true
        end
    end, State),
    % notify other modules
    lists:foreach(fun({_ ,Opts}) ->
        {value, {_, CallBMod}} = lists:keysearch(modsrv, 1, Opts),
        CallBMod:modsrv_notify(Arg)
    end, NewState),
    {reply, ok, NewState};

handle_call(dump, _From, State) ->
    {reply, State, State};

% @private
handle_call(_R, _F, S) ->
    {noreply, S}.

% @private
handle_cast(_R, S) ->
    {noreply, S}.

% @private
handle_info(_I, S) ->
    {noreply, S}.

% @private
terminate(_R, _S) ->
    normal.

% @private
code_change(_O, S, _E) ->
    {ok, S}.




% TESTS
% @private eunit test
add_test() ->
    ?assert(?MODULE:get_modules() == []),
    ?assert(?MODULE:hello({mod_a, [{modsrv_callback, cbmod1}, {othertuple, b}]}) == ok),
    ?assert(?MODULE:get_modules() == [{mod_a, [{modsrv_callback, cbmod1}, {othertuple, b}]}]).

% @private eunit test
modify_test() ->
    ?assert(?MODULE:hello({mod_a, [{modsrv_callback, cbmod2}, {othertuple, a}]}) == ok),
    ?assert(?MODULE:get_modules() == [{mod_a, [{modsrv_callback, cbmod2}, {othertuple,a}]}]).
    
% @private eunit test
del_test() ->
    ?assert(?MODULE:bye(mod_a) == ok),
    ?assert(?MODULE:get_modules() == []).

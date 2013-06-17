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
% @copyright 2012-2013 <Sebastien Serre sserre.bx@gmail.com>
-module(tracker_target_store).
-behaviour(gen_server).
-include("../include/tracker.hrl").


% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

% Gen server utils
-export([
    start_link/1,
    clear_locks/0,
    dump_state/0
]).

% Server API Should only be accessible from the system
-export([
    create_target/1,
    update_target/1,
    delete_target/1,
    new/1,
    info/0
]).

-record(state, {
    db_dir,
    db_file,
    db_name
}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SERVER UTILITY                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @private
-spec start_link(string()) -> {ok, pid()} | {error, any()}.
start_link(DbDir) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [DbDir], []).

% @private
-spec dump_state() -> ok.
dump_state() ->
    gen_server:call(?MODULE, dump_state).

-spec clear_locks() -> ok | {error, any()}.
% @doc
% Delete all entry where only the Id is set. It can append when a client 
% activity is interupted after a new() command.
% This command is executed at boot time.
% @end
clear_locks() ->
    gen_server:call(?MODULE, clear_locks).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MODULE API. NOT ALL FUN NEED TO BE EXPORTED TO THE CLIENT                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new(Target) ->
    gen_server:call(?MODULE, {create_target, Target}).

-spec create_target(#target{}) -> {ok, target_id()}.
% @doc
% Create a new target.
% @end
create_target(Target) ->
    gen_server:call(?MODULE, {create_target, Target}).

-spec update_target(#target{}) -> ok | {error, any()}.
% @doc
% Replace an existing target record.
% @end
update_target(Target) ->
    gen_server:call(?MODULE, {update_target, Target}).


-spec delete_target(#target{}) -> ok | {error, any()}.
% @doc
% Delete a target from the database. Return ok | {error, Reason} where Reason
% is the error returned by dets:delete(Name,Key).
% @end
delete_target(Target) ->
    gen_server:call(?MODULE, {delete_target, Target}).
    

-spec info() -> [#target{}].
% @doc
% Return a list of all target records registered.
% @end
info() ->
    gen_server:call(?MODULE, info).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CALLBACKS                                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @private
init([DbDir]) ->
    DbDir2 = filename:absname(DbDir),
    DbFile = filename:absname_join(DbDir2, "target_db"),
    DbName = target_db,
    {ok, target_db} = dets:open_file(DbName, [
            {file,      DbFile},
            {access,    read_write},
            {type,      set},
            {keypos,    2}]),    % we will store records
    {ok, #state{
            db_dir  = DbDir2,
            db_file = DbFile,
            db_name = DbName}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @private
handle_call({create_target, Target}, _F, 
        #state{db_name = DbName} = S) ->
    Id = dbwrite_create_target(DbName, Target),
    {reply, {ok, Id}, S};

handle_call({update_target, Target}, _F, 
        #state{db_name = DbName} = S) ->
    Id = dbwrite_update_target(DbName, Target),
    {reply, {ok, Id}, S};

handle_call({delete_target, Target}, _F, 
        #state{db_name = DbName} = S) ->
    Rep = dbwrite_delete_target(Target, DbName),
    {reply, Rep, S};

handle_call(info, _F, #state{db_name = DbName} = S) ->
    Rep = dbread_info(DbName),
    {reply, Rep, S};

handle_call(dump_state, _F, #state{db_name = DbName} = S) ->
    Rep = dets:match(DbName, '$1'),
    {reply, Rep, S};

handle_call(Q, _F, S) ->
    log("handle_call unknown msg: ~p ~p ~p", [?MODULE, ?LINE, Q]),
    {reply, unknown_command, S}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast(stop, S) ->
    {stop, normal, S};

handle_cast(Q, S) ->
    log("handle_cast unknown ~p ~p ~p", [?MODULE, ?LINE, Q]),
    {noreply, S}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @private
handle_info(I,S) ->
    log("handle_info unknown ~p ~p ~p", [?MODULE, ?LINE, I]),
    {noreply, S}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @private
terminate(_R, #state{db_name = DbName}) ->
    dets:sync(DbName),
    dets:close(DbName),
    normal.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @private
code_change(_O, S, _E) ->
    {ok, S}.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRIVATE FUNCTS                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec dbwrite_create_target(atom(), #target{}) -> target_id().
dbwrite_create_target(DbName, Target) ->
    Id = tracker_misc:generate_id(),
    NewTarget = Target#target{id = Id},
    case dets:insert_new(DbName, NewTarget) of
        true    ->
            dets:sync(DbName),
            generate_event({insert, NewTarget}),
            Id;
        false   ->
            dbwrite_create_target(DbName, Target)
    end.

-spec dbwrite_update_target(#target{}, atom()) -> ok | {error, any()}.
dbwrite_update_target(Target, DbName) ->
    case dets:insert(DbName, Target) of
        ok      -> dets:sync(DbName);
        Other   -> Other
    end.


-spec dbwrite_delete_target(#target{}, atom()) -> ok | {error, any()}.
dbwrite_delete_target(#target{id = Id}, DbName) ->
    case dets:delete(DbName, Id) of
        ok ->
            dets:sync(DbName),
            generate_event({delete, Id});
        OtherReturn ->
            OtherReturn
    end.


-spec dbread_info(atom()) -> [#target{}].
dbread_info(DbName) ->
    lists:flatten(dets:match(DbName, '$1')).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPERS                                                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_event(Event) ->
    gen_event:notify(tracker_events, Event).

log(tracker_events, Event) ->
    io:format("~p~n", [Event]);

log(A, B) ->
    io:format(A, B).

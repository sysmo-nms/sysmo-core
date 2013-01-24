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
-module(targets).
-behaviour(gen_server).
-include_lib("../include/targets.hrl").

-record(tserver_state, {
    db_dir,
    db_file,
    db_name
}).

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

% Client API
-export([
    new/0,
    set_ip/2,
    set_hostname/2,
    set_property/2,
    set_tag/2,
    del_tag/2,
    del_target/1,
    del_property/2,
    info/0,
    info/1,
    get_ids/0,
    get_ip/1,
    get_hostname/1,
    get_tags/1,
    get_property/2,
    possess_tag/2,
    possess_property/3,
    filter/2,
    filter/3
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SERVER UTILITY                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @private
-spec start_link(string) -> {ok, pid()} | {error, any()}.
start_link(DbDir) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [DbDir], []).

% @private
-spec dump_state() -> ok.
dump_state() ->
    gen_server:cast(?MODULE, dump_state).

-spec clear_locks() -> ok | {error, any()}.
% @doc
% Delete all entry where only the Id is set. It can append when a client 
% activity is interupted after a new() command.
% This command is executed at boot time.
% @end
clear_locks() ->
    gen_server:call(?MODULE, clear_locks).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CLIENTS API                                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type target_id()   :: string().
-type ip_address()  :: {integer(), integer(), integer(), integer()}.
-type hostname()    :: string().
-type dets_name()   :: atom().
-type property_key()    :: atom().
-type property_val()    :: any().
-type property()    :: {property_key(), property_val()}.
-type tag()         :: any().


-spec new() -> target_id().
% @doc
% Lock a new id in the database and return it's value. Later call to configure
% the entry will be donne with this id.
% @end
new() ->
    gen_server:call(?MODULE, lock_id).


-spec set_ip(target_id(), ip_address()) -> 
        ok | {error, bad_formating | unknown_id}.
% @doc
% Set the record element ip to the value Ip. Did not check if the address is
% allready used by another entry.
% @end
set_ip(Id, Ip) ->
    gen_server:call(?MODULE, {set_ip, Id, Ip}).


-spec set_hostname(target_id(), hostname()) ->
        ok | {error, bad_formating | unknown_id}.
% @doc
% Set the record element hostname to the value Hostname. Did not check if the 
% name is allready used. Return "bad_formating" only if Hostname is not a
% string().
% @end
set_hostname(Id, Hostname) ->
    gen_server:call(?MODULE, {set_hostname, Id, Hostname}).


-spec set_property(target_id(), property()) ->
        ok | {error, any()}.
% @doc
% Add an element {property_key(), property_val()} to the list of the property
% of the element. Overwrite if another property_key() exist.
% @end
set_property(Id, Property) ->
    gen_server:call(?MODULE, {set_property, Id, Property}).


-spec set_tag(target_id(), tag()) -> ok | {error, unknown_id}.
% @doc
% Add a new tag to the "lazy_tag" list of the entry. If the tag allready exist
% the result will have the same value.
% @end
set_tag(Id, Tag) ->
    gen_server:call(?MODULE, {set_tag, Id, Tag}).

%% del
del_target(Id) ->
    Rep =   gen_server:call(?MODULE, {del_target, Id}),
    Rep.
    
del_property(Id, Property) ->
    Rep =   gen_server:call(?MODULE, {del_property, Id, Property}),
    Rep.

del_tag(Id, Tag) ->
    Rep =   gen_server:call(?MODULE, {del_tag, Id, Tag}),
    Rep.

%% get
info() ->
    TargetList   = gen_server:call(?MODULE, info),
    TargetList.

info(Id) ->
    Record = gen_server:call(?MODULE, {info, Id}),
    Record.

% return all ids
get_ids() ->
    IdList = gen_server:call(?MODULE, {get_id}),
    IdList.

get_ip(Id) ->
    Ip =    gen_server:call(?MODULE, {get_ip, Id}),
    Ip.

get_hostname(Id) ->
    Hostname = gen_server:call(?MODULE, {get_hostname, Id}),
    Hostname.

get_tags(Id) ->
    TagsList = gen_server:call(?MODULE, {get_tags, Id}),
    TagsList.

get_property(Id, PropertyKey) ->
    PropertyVal = gen_server:call(?MODULE, {get_property, Id, PropertyKey}),
    PropertyVal.

possess_tag(Id, Tag) ->
    Bool = gen_server:call(?MODULE, {possess_tag, Id, Tag}),
    Bool.

% Type = property_key | property_tuple
possess_property(Type, Id, Arg) ->
    Bool = gen_server:call(?MODULE, {possess_property, Type, Id, Arg}),
    Bool.

% Type = tag | property_key | property_tuple
filter(Type, ArgList) ->
    TargetList = gen_server:call(?MODULE, {filter, all, Type, ArgList}),
    TargetList.

% Type = tag | property_key | property_tuple
filter(TargetIds, Type, ArgList) ->
    TargetList = gen_server:call(?MODULE, {filter, TargetIds, Type, ArgList}),
    TargetList.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CALLBACKS                                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([DbDir]) ->
    DbDir2 = filename:absname(DbDir),
    DbFile = filename:absname_join(DbDir2, "targets_db"),
    DbName = targets_db,
    {ok, targets_db} = dets:open_file(DbName, [
            {file, DbFile},
            {access, read_write},
            {type, set},
            {keypos, 2},    % we will store records
            {repair, force}]),
    dbwrite_clear_locks(DbName),
    {ok, #tserver_state{
            db_dir  = DbDir2,
            db_file = DbFile,
            db_name = DbName}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @private
handle_call(lock_id, _F, #tserver_state{db_name = DbName} = S) ->
    Id = dbwrite_lock_id(DbName),
    log("lock_id ~p ~n", [Id]),
    {reply, Id, S};

handle_call(clear_locks, _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbwrite_clear_locks(DbName),
    log("clear_locks ~p ~p~n", [Rep]),
    {reply, Rep, S};

handle_call({set_ip, Id, Ip}, _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbwrite_set_ip(Id, Ip, DbName),
    log("set_ip ~p ~p ~p~n", [Id, Ip, DbName]),
    {reply, Rep, S};

handle_call({set_hostname, Id, Hostname}, 
        _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbwrite_set_hostname(Id, Hostname, DbName),
    log("set_hostname ~p ~p ~p~n", [Id, Hostname, DbName]),
    {reply, Rep, S};

handle_call({set_property, Id, Property},
        _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbwrite_set_property(Id, Property, DbName),
    log("set_property ~p ~p ~p ~n", [Id, Property, DbName]),
    {reply, Rep, S};

handle_call({set_tag, Id, Tag}, _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbwrite_set_tag(Id, Tag, DbName),
    log("add_tag ~p ~p ~p~n", [Id, Tag, DbName]),
    {reply, Rep, S};

handle_call({del_property, Id, Property},
        _F, #tserver_state{db_name = DbName} = S) ->
    log("del_property ~p ~p ~p~n", [Id, Property, DbName]),
    {reply, ok, S};

handle_call({del_tag, Id, Tag}, _F, #tserver_state{db_name = DbName} = S) ->
    log("del_tag ~p ~p ~p~n", [Id, Tag, DbName]),
    {reply, ok, S};

handle_call({del_target, Id}, _F, #tserver_state{db_name = DbName} = S) ->
    log("del_target ~p ~p~n", [Id, DbName]),
    {reply, ok, S};

handle_call(info, _F, #tserver_state{db_name = DbName} = S) ->
    log("info/0 ~p~n", [DbName]),
    {reply, tableRowsAsList, S};

handle_call({info, Id}, _F, #tserver_state{db_name = DbName} = S) ->
    log("info/1 ~p ~p~n", [Id, DbName]),
    {reply, target_record, S};

handle_call(get_ids, _F, #tserver_state{db_name = DbName} = S) ->
    log("get_ids ~p~n", [DbName]),
    {reply, target_ip, S};

handle_call({get_ip, Id}, _F, #tserver_state{db_name = DbName} = S) ->
    log("get_ip ~p ~p~n", [Id, DbName]),
    {reply, target_ip, S};

handle_call({get_hostname, Id}, _F, #tserver_state{db_name = DbName} = S) ->
    log("get_hostname ~p ~p~n", [Id, DbName]),
    {reply, target_hostname, S};

handle_call({get_tags, Id}, _F, #tserver_state{db_name = DbName} = S) ->
    log("get_tags ~p ~p~n", [Id, DbName]),
    {reply, tag_list, S};

handle_call({get_property, Id}, _F, #tserver_state{db_name = DbName} = S) ->
    log("get_property ~p ~p~n", [Id, DbName]),
    {reply, property_value, S};

handle_call({possess_tag, Id, Tag}, 
        _F, #tserver_state{db_name = DbName} = S) ->
    log("possess_tag ~p ~p ~p~n", [Id, Tag, DbName]),
    {reply, bolean, S};

handle_call({possess_property, Type, Id, Arg}, 
        _F, #tserver_state{db_name = DbName} = S) ->
    log("possess_property ~p ~p ~p ~p~n", [Type, Id, Arg, DbName]),
    {reply, bolean, S};

handle_call({filter, Targets, Type, ArgList}, 
        _F, #tserver_state{db_name = DbName} = S) ->
    log("filter ~p ~p ~p ~p~n", [Targets, Type, ArgList, DbName]),
    {reply, target_list, S};

handle_call(Q, _F, S) ->
    log("handle_call unknown msg: ~p ~p ~p", [?MODULE, ?LINE, Q]),
    {reply, unknown_command, S}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @private
handle_cast(dump_state, #tserver_state{db_name = DbName} = S) ->
    log("state is ~p~n", [S]),
    log("targets_db is: ~n~p~n", [dets:match(DbName, '$1')]),
    {noreply, S};

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
terminate(_R, _S) ->
    normal.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @private
code_change(_O, S, _E) ->
    {ok, S}.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRIVATE FUNCTS                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec dbwrite_lock_id(dets_name()) -> target_id().
dbwrite_lock_id(DetsName) ->
    Id = "target-" ++ targets_misc:generate_id(),
    % if Id exist in the table retry else return.
    case dets:insert_new(DetsName, #target{id = Id}) of
        true    ->
            Id;
        false   ->
            dbwrite_lock_id(DetsName)
    end.

-spec dbwrite_clear_locks(dets_name()) -> ok | {error, any()}.
dbwrite_clear_locks(DbName) ->
    Rep = dets:match_delete(DbName, {target, '_', undef, undef, [], []}),
    Rep.

-spec dbwrite_set_ip(target_id(), ip_address(), dets_name()) 
    -> ok | {error, bad_formating | unknown_id}.
dbwrite_set_ip(Id, Ip, DbName) ->
    case dets:lookup(DbName, Id) of
        [Record] -> 
            case is_ip(Ip) of
                true ->
                    UpdatedRecord = Record#target{ip = Ip},
                    dets:insert(DbName, UpdatedRecord);
                false ->
                    {error, bad_formating}
            end;
        [] ->
            {error, unknown_id}
    end.

-spec dbwrite_set_hostname(target_id(), hostname(), dets_name()) 
    -> ok | {error, bad_name | unknown_id}.
dbwrite_set_hostname(Id, Hostname, DbName) ->
    case dets:lookup(DbName, Id) of
        [Record] -> 
            case is_string(Hostname) of
                true ->
                    UpdatedRecord = Record#target{hostname = Hostname},
                    dets:insert(DbName, UpdatedRecord);
                false ->
                    {error, bad_name}
            end;
        [] ->
            {error, unknown_id}
    end.

-spec dbwrite_set_property(target_id(), property(), dets_name()) 
    -> ok | {error, bad_property | unknown_id}.
dbwrite_set_property(Id, Property, DbName) ->
    case dets:lookup(DbName, Id) of
        [Record] -> 
            case Property of
                {Key, _} ->
                    UpdatedProperties = 
                        lists:keydelete(Key, 1, Record#target.properties),
                    degs:insert(DbName, Record#target{
                            properties = [Property | UpdatedProperties]});
                _ ->
                    {error, bad_property}
            end;
        [] ->
            {error, unknown_id}
    end.


-spec dbwrite_set_tag(target_id(), tag(), dets_name()) 
    -> ok | {error, unknown_id}.
dbwrite_set_tag(Id, Tag, DbName) ->
    case dets:lookup(DbName, Id) of
        [Record] -> 
            TagList = Record#target.lazy_tags,
            case lists:member(Tag, TagList) of
                true    -> ok;
                false   ->
                    NewRecord = Record#target{lazy_tags = [Tag | TagList]},
                    dets:insert(DbName, NewRecord)
            end;
        [] ->
            {error, unknown_id}
    end.


-spec is_ip(ip_address()) -> boolean().
is_ip(_Ip) ->
    true.

-spec is_string(any()) -> boolean().
is_string(Arg) ->
    Fun = fun(X) ->
        if 
            X < 0   -> false;
            X > 255 -> false;
            true    -> true
        end
    end,
    case is_list(Arg) of
        true ->
            lists:all(Fun, Arg);
        false ->
            false
    end.



-spec log(string(), list()) -> ok.
log(A, B) ->
    io:format(A, B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EUNIT TESTS                                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


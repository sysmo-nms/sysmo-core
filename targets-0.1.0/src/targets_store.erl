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
% @copyright 2012-2013 <Sebastien Serre sserre.bx@gmail.com>
% @doc
% <p>
% Enms data store. This module contain any informations related to node 
% ("targets") configuration and an API. The data type are #target{} records, 
% stored in a dets file.
% </p>
% <h4>Here is the initial target record:</h4>
% #target{id = undef, ip = undef, hostname = undef, sys_properties = [], 
% sys_tags = [], properties = [], tags = []}
% <p>
% Note that sys_properties and sys_tags are only used by the system. properties
% and tags are user definissable.
% </p>
% @end
-module(targets_store).
-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").
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

% Server API Should only be accessible from the system
-export([
    set_sys_tag/2,
    set_sys_property/2,
    del_sys_tag/2,
    del_sys_property/2,
    get_sys_tags/1,
    get_sys_property/2,
    get_sys_properties/1,
    possess_sys_tag/2,
    possess_sys_property/3
]).

% Client and server API
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
    get_properties/1,
    possess_tag/2,
    possess_property/3,
    filter/1,
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
% MODULE API. NOT ALL FUN NEED TO BE EXPORTED TO THE CLIENT                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type target_id()   :: atom().
-type ip_address()  :: {integer(), integer(), integer(), integer()}.
-type hostname()    :: string().
-type dets_name()   :: atom().
-type property_key()    :: atom().
-type property_val()    :: any().
-type property()    :: {property_key(), property_val()}.
-type tag()         :: any().


-spec new() -> {ok, target_id()}.
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
% valid_hostname_string().
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

-spec set_sys_property(target_id(), property()) ->
        ok | {error, any()}.
% @doc
% Add an element {property_key(), property_val()} to the list of the 
% sys_property of the element. Overwrite if another property_key() exist.
% Should not be exported to users. Reserved for system use.
% @end
set_sys_property(Id, Property) ->
    gen_server:call(?MODULE, {set_sys_property, Id, Property}).

-spec set_tag(target_id(), tag()) -> ok | {error, unknown_id}.
% @doc
% Add a new tag to the "tag" list of the entry. If the tag allready exist
% the result will have the same value.
% @end
set_tag(Id, Tag) ->
    gen_server:call(?MODULE, {set_tag, Id, Tag}).

-spec set_sys_tag(target_id(), tag()) -> ok | {error, unknown_id}.
% @doc
% Add a new tag to the sys_tag list of the entry. If the tag allready exist
% the result will have the same value.
% Should not be exported to users. Reserved for system use.
% @end
set_sys_tag(Id, Tag) ->
    gen_server:call(?MODULE, {set_sys_tag, Id, Tag}).

-spec del_target(target_id()) -> ok | {error, any()}.
% @doc
% Delete a target from the database. Return ok | {error, Reason} where Reason
% is the error returned by dets:delete(Name,Key).
% @end
del_target(Id) ->
    gen_server:call(?MODULE, {del_target, Id}).
    

-spec del_property(target_id(), property_key()) -> ok | {error, any()}.
% @doc
% Delete a property by its key name. Return ok even if the property key did
% not exist.
% @end
del_property(Id, Property) ->
    gen_server:call(?MODULE, {del_property, Id, Property}).

-spec del_sys_property(target_id(), property_key()) -> ok | {error, any()}.
% @doc
% Delete a property by its key name of the sys_property list. Return ok even 
% if the property key did not exist.
% Should not be exported to users. Reserved for system use.
% @end
del_sys_property(Id, Property) ->
    gen_server:call(?MODULE, {del_sys_property, Id, Property}).

-spec del_tag(target_id(), tag()) -> ok | {error, any()}.
% @doc
% Delete a tag from the tag list. Return ok event if the tag did not exist.
% @end
del_tag(Id, Tag) ->
    gen_server:call(?MODULE, {del_tag, Id, Tag}).

-spec del_sys_tag(target_id(), tag()) -> ok | {error, any()}.
% @doc
% Delete a tag from the sys_tag list. Return ok event if the tag did not
% exist. Should not be exported to users. Reserved for system use.
% @end
del_sys_tag(Id, Tag) ->
    gen_server:call(?MODULE, {del_sys_tag, Id, Tag}).

-spec info() -> [#target{}].
% @doc
% Return a list of all target records registered.
% @end
info() ->
    gen_server:call(?MODULE, info).

-spec info(target_id()) -> {ok, #target{}} | {error, unknown_id}.
% @doc
% Return the target record wich match the id Id.
% @end
info(Id) ->
    gen_server:call(?MODULE, {info, Id}).

-spec get_ids() -> [target_id()].
% @doc
% Return a list containing all ids.
% @end
get_ids() ->
    gen_server:call(?MODULE, get_ids).

-spec get_ip(target_id())
        -> {ok, ip_address() | undef} | {error | any()}.
% @doc
% Return the address ip of the target specified by Id.
% @end
get_ip(Id) ->
    gen_server:call(?MODULE, {get_ip, Id}).

-spec get_hostname(target_id()) 
        -> {ok, hostname() | undef} | {error | any()}.
% @doc
% Return the hostname of the target specified by Id.
% @end
get_hostname(Id) ->
    gen_server:call(?MODULE, {get_hostname, Id}).

-spec get_tags(target_id()) 
        -> {ok, [tag()]} | {error | any()}.
% @doc
% Return the tag list of the target specified by Id.
% @end
get_tags(Id) ->
    gen_server:call(?MODULE, {get_tags, Id}).

-spec get_sys_tags(target_id()) 
        -> {ok, [tag()]} | {error | any()}.
% @doc
% Return the tag list of the target specified by Id.
% Should not be exported to users. Reserved for system use.
% @end
get_sys_tags(Id) ->
    gen_server:call(?MODULE, {get_sys_tags, Id}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_property(target_id(), property_key())
        -> {ok, property_val() | false} | {error, unknown_id}.
% @doc
% Get property_val() with key property_key() from the target specified by Id.
% @end
get_property(Id, PropertyKey) ->
    gen_server:call(?MODULE, {get_property, Id, PropertyKey}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_sys_property(target_id(), property_key())
        -> {ok, property_val() | false} | {error, unknown_id}.
% @doc
% Get property_val() with key property_key() from the target specified by Id.
% Should not be exported to users. Reserved for system use.
% @end
get_sys_property(Id, PropertyKey) ->
    gen_server:call(?MODULE, {get_sys_property, Id, PropertyKey}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_properties(target_id())
        -> [property()] | {error, unknown_id}.
% @doc
% Return the properties list from the target defined by target_id().
% @end
get_properties(Id) ->
    gen_server:call(?MODULE, {get_properties, Id}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_sys_properties(target_id())
        -> {ok, property_val() | false} | {error, unknown_id}.
% @doc
% Return the sys_properties list from the target defined by target_id().
% Should not be exported to users. Reserved for system use.
% @end
get_sys_properties(Id) ->
    gen_server:call(?MODULE, {get_sys_properties, Id}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec possess_tag(target_id(), tag())
        -> boolean() | {error, unknown_id}.
% @doc
% Return true if the tag Tag is found in the target tag list.
% @end
possess_tag(Id, Tag) ->
    gen_server:call(?MODULE, {possess_tag, Id, Tag}).

-spec possess_sys_tag(target_id(), tag())
        -> boolean() | {error, unknown_id}.
% @doc
% Return true if the ...tag Tag... is found in the target sys_tag list.
% Should not be exported to users. Reserved for system use.
% @end
possess_sys_tag(Id, Tag) ->
    gen_server:call(?MODULE, {possess_sys_tag, Id, Tag}).

-spec possess_property(
    property_key | property_tuple,
    target_id(),
    property() | property_key())
        -> boolean() | {error, unknown_id}.
% @doc
% Search the property list of the target spcified by Id.
% if Type = property_key, Arg must be a property_key(),
% if Type = property, Arg must be a property() tuple.
% @end
possess_property(Type, Id, Arg) ->
    gen_server:call(?MODULE, {possess_property, Type, Id, Arg}).

-spec possess_sys_property(
    property_key | property_tuple,
    target_id(),
    property() | property_key())
        -> boolean() | {error, unknown_id}.
% @doc
% Search the sys_property list of the target spcified by Id.
% if Type = property_key, Arg must be a property_key(),
% if Type = property, Arg must be a property() tuple.
% Should not be exported to users. Reserved for system use.
% @end
possess_sys_property(Type, Id, Arg) ->
    gen_server:call(?MODULE, {possess_sys_property, Type, Id, Arg}).


-spec filter(
    [
        {tag, tag()} | 
        {property_key,   property_key()} |
        {property_tuple, property()}
    ])
        -> [#target{}].
% @doc
% Return a list of #target{} wich possess every elements specified in Arglist.
% @end
filter(ArgList) ->
    gen_server:call(?MODULE, {filter, all, ArgList}).

% Type = tag | property_key | property_tuple
filter(TargetIds, Type, ArgList) ->
    TargetList = gen_server:call(?MODULE, {filter, TargetIds, Type, ArgList}),
    TargetList.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CALLBACKS                                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @private
init([DbDir]) ->
    DbDir2 = filename:absname(DbDir),
    DbFile = filename:absname_join(DbDir2, "targets_db"),
    DbName = targets_db,
    {ok, targets_db} = dets:open_file(DbName, [
            {file, DbFile},
            {access, read_write},
            {type, set},
            {keypos, 2}]),    % we will store records
    %dbwrite_clear_locks(DbName),
    {ok, #tserver_state{
            db_dir  = DbDir2,
            db_file = DbFile,
            db_name = DbName}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @private
handle_call(lock_id, _F, #tserver_state{db_name = DbName} = S) ->
    Id = dbwrite_lock_id(DbName),
    {reply, {ok, Id}, S};

handle_call(clear_locks, _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbwrite_clear_locks(DbName),
    {reply, Rep, S};

handle_call({set_ip, Id, Ip}, _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbwrite_set_ip(Id, Ip, DbName),
    {reply, Rep, S};

handle_call({set_hostname, Id, Hostname}, 
        _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbwrite_set_hostname(Id, Hostname, DbName),
    {reply, Rep, S};

handle_call({set_property, Id, Property},
        _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbwrite_set_property(Id, Property, DbName),
    {reply, Rep, S};

handle_call({set_sys_property, Id, Property},
        _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbwrite_set_sys_property(Id, Property, DbName),
    {reply, Rep, S};

handle_call({set_tag, Id, Tag}, _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbwrite_set_tag(Id, Tag, DbName),
    {reply, Rep, S};

handle_call({set_sys_tag, Id, Tag}, _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbwrite_set_sys_tag(Id, Tag, DbName),
    {reply, Rep, S};

handle_call({del_target, Id}, _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbwrite_del_target(Id, DbName),
    {reply, Rep, S};

handle_call({del_property, Id, PropertyKey},
        _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbwrite_del_property(Id, PropertyKey, DbName),
    {reply, Rep, S};

handle_call({del_sys_property, Id, PropertyKey},
        _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbwrite_del_sys_property(Id, PropertyKey, DbName),
    {reply, Rep, S};

handle_call({del_tag, Id, Tag}, _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbwrite_del_tag(Id, Tag, DbName),
    {reply, Rep, S};

handle_call({del_sys_tag, Id, Tag}, _F, 
        #tserver_state{db_name = DbName} = S) ->
    Rep = dbwrite_del_sys_tag(Id, Tag, DbName),
    {reply, Rep, S};

handle_call(info, _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbread_info(DbName),
    {reply, Rep, S};

handle_call({info, Id}, _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbread_info(Id, DbName),
    {reply, Rep, S};

handle_call(get_ids, _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbread_get_ids(DbName),
    {reply, Rep, S};

handle_call({get_ip, Id}, _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbread_get_ip(Id, DbName),
    {reply, Rep, S};

handle_call({get_hostname, Id}, _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbread_get_hostname(Id, DbName),
    {reply, Rep, S};

handle_call({get_tags, Id}, _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbread_get_tags(Id, DbName),
    {reply, Rep, S};

handle_call({get_sys_tags, Id}, _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbread_get_sys_tags(Id, DbName),
    {reply, Rep, S};

handle_call({get_property, Id, PropertyKey}, _F, 
        #tserver_state{db_name = DbName} = S) ->
    Rep = dbread_get_property(Id, PropertyKey, DbName),
    {reply, Rep, S};

handle_call({get_sys_property, Id, PropertyKey}, _F, 
        #tserver_state{db_name = DbName} = S) ->
    Rep = dbread_get_sys_property(Id, PropertyKey, DbName),
    {reply, Rep, S};

handle_call({get_properties, Id}, _F, 
        #tserver_state{db_name = DbName} = S) ->
    Rep = dbread_get_properties(Id, DbName),
    {reply, Rep, S};

handle_call({get_sys_properties, Id}, _F, 
        #tserver_state{db_name = DbName} = S) ->
    Rep = dbread_get_sys_properties(Id, DbName),
    {reply, Rep, S};

handle_call({possess_tag, Id, Tag}, 
        _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbread_possess_tag(Id, Tag, DbName),
    {reply, Rep, S};

handle_call({possess_sys_tag, Id, Tag}, 
        _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbread_possess_sys_tag(Id, Tag, DbName),
    {reply, Rep, S};

handle_call({possess_property, Type, Id, Arg}, 
        _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbread_possess_property(Type, Id, Arg, DbName),
    {reply, Rep, S};

handle_call({possess_sys_property, Type, Id, Arg}, 
        _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbread_possess_sys_property(Type, Id, Arg, DbName),
    {reply, Rep, S};

handle_call({filter, Targets, ArgList}, 
        _F, #tserver_state{db_name = DbName} = S) ->
    Rep = dbread_filter(Targets, ArgList, DbName),
    {reply, Rep, S};

handle_call(Q, _F, S) ->
    log("handle_call unknown msg: ~p ~p ~p", [?MODULE, ?LINE, Q]),
    {reply, unknown_command, S}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @private
handle_cast(dump_state, #tserver_state{db_name = DbName} = S) ->
    log("state is ~p~n", [S]),
    log("targets_db is: ~n~p~n", [dets:match(DbName, '$1')]),
    {noreply, S};

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
    Id = targets_misc:generate_id(),
    % if Id exist in the table retry else return.
    TargetRecord = #target{id = Id},
    case dets:insert_new(DetsName, TargetRecord) of
        true    ->
            generate_event({insert, TargetRecord}),
            Id;
        false   ->
            dbwrite_lock_id(DetsName)
    end.

-spec dbwrite_clear_locks(dets_name()) -> ok | {error, any()}.
dbwrite_clear_locks(DbName) ->
    Entrys = dets:match(DbName, 
            {target, '$1', undef, undef, [], [], [], []}),
    lists:foreach(fun(X) -> 
        generate_event({delete, X})
    end, Entrys),
    Rep = dets:match_delete(DbName, 
            {target, '_', undef, undef, [], [], [], []}),
    Rep.

-spec dbwrite_set_ip(target_id(), ip_address(), dets_name()) 
        -> ok | {error, bad_formating | unknown_id}.
dbwrite_set_ip(Id, Ip, DbName) ->
    case dets:lookup(DbName, Id) of
        [Record] -> 
            case is_ip(Ip) of
                true ->
                    UpdatedRecord = Record#target{ip = Ip},
                    case dets:insert(DbName, UpdatedRecord) of
                        ok ->
                            generate_event({update, UpdatedRecord});
                        OtherReturn ->
                            OtherReturn
                    end;
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
            case valid_hostname_string(Hostname) of
                true ->
                    UpdatedRecord = Record#target{hostname = Hostname},
                    case dets:insert(DbName, UpdatedRecord) of
                        ok ->
                            generate_event({update, UpdatedRecord});
                        OtherReturn ->
                            OtherReturn
                    end;
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
                    UpdatedRecord = Record#target{
                            properties = [Property | UpdatedProperties]},
                    case dets:insert(DbName, UpdatedRecord) of
                        ok ->
                            generate_event({update, UpdatedRecord});
                        OtherReturn ->
                            OtherReturn
                    end;
                _ ->
                    {error, bad_property}
            end;
        [] ->
            {error, unknown_id}
    end.


-spec dbwrite_set_sys_property(target_id(), property(), dets_name()) 
        -> ok | {error, bad_property | unknown_id}.
dbwrite_set_sys_property(Id, Property, DbName) ->
    case dets:lookup(DbName, Id) of
        [Record] -> 
            case Property of
                {Key, _} ->
                    UpdatedProperties = 
                        lists:keydelete(Key, 1, Record#target.sys_properties),
                    UpdatedRecord = Record#target{
                        sys_properties = [Property | UpdatedProperties]},
                    case dets:insert(DbName, UpdatedRecord) of
                        ok ->
                            generate_event({update, UpdatedRecord});
                        OtherReturn ->
                            OtherReturn
                    end;
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
            TagList = Record#target.tags,
            case lists:member(Tag, TagList) of
                true    -> ok;
                false   ->
                    UpdatedRecord = Record#target{tags = [Tag | TagList]},
                    case dets:insert(DbName, UpdatedRecord) of
                        ok ->
                            generate_event({update, UpdatedRecord});
                        OtherReturn ->
                            OtherReturn
                    end
            end;
        [] ->
            {error, unknown_id}
    end.


-spec dbwrite_set_sys_tag(target_id(), tag(), dets_name()) 
        -> ok | {error, unknown_id}.
dbwrite_set_sys_tag(Id, Tag, DbName) ->
    case dets:lookup(DbName, Id) of
        [Record] -> 
            TagList = Record#target.sys_tags,
            case lists:member(Tag, TagList) of
                true    -> ok;
                false   ->
                    UpdatedRecord = Record#target{sys_tags = [Tag | TagList]},
                    case dets:insert(DbName, UpdatedRecord) of
                        ok ->
                            generate_event({update, UpdatedRecord});
                        OtherReturn ->
                            OtherReturn
                    end
            end;
        [] ->
            {error, unknown_id}
    end.


-spec dbwrite_del_target(target_id(), dets_name()) 
        -> ok | {error, any()}.
dbwrite_del_target(Id, DbName) ->
    case dets:delete(DbName, Id) of
        ok ->
            generate_event({delete, Id});
        OtherReturn ->
            OtherReturn
    end.


-spec dbwrite_del_property(target_id(), property_key(), dets_name())
        -> ok | {error, any()}.
dbwrite_del_property(Id, PropertyKey, DbName) ->
    case dets:lookup(DbName, Id) of
        [Record] ->
            Properties = Record#target.properties,
            UpdatedProperties = lists:keydelete(PropertyKey, 1, Properties),
            UpdatedRecord = Record#target{properties = UpdatedProperties},
            case dets:insert(DbName, UpdatedRecord) of
                ok ->
                    generate_event({update, UpdatedRecord});
                OtherReturn ->
                    OtherReturn
            end;
        [] ->
            {error, unknown_id}
    end.

-spec dbwrite_del_sys_property(target_id(), property_key(), dets_name())
        -> ok | {error, any()}.
dbwrite_del_sys_property(Id, PropertyKey, DbName) ->
    case dets:lookup(DbName, Id) of
        [Record] ->
            Properties = Record#target.sys_properties,
            UpdatedProperties = lists:keydelete(PropertyKey, 1, Properties),
            UpdatedRecord = Record#target{sys_properties = UpdatedProperties},
            case dets:insert(DbName, UpdatedRecord) of
                ok ->
                    generate_event({update, UpdatedRecord});
                OtherReturn ->
                    OtherReturn
            end;
                    
        [] ->
            {error, unknown_id}
    end.

-spec dbwrite_del_tag(target_id(), tag(), dets_name()) -> ok | {error, any()}.
dbwrite_del_tag(Id, Tag, DbName) ->
    case dets:lookup(DbName, Id) of
        [Record] ->
            TagList     = Record#target.tags,
            UpdatedTagList  = lists:delete(Tag, TagList),
            UpdatedRecord   = Record#target{tags = UpdatedTagList},
            case dets:insert(DbName, UpdatedRecord) of
                ok ->
                    generate_event({update, UpdatedRecord});
                OtherReturn ->
                    OtherReturn
            end;
        [] ->
            {error, unknown_id}
    end.

-spec dbwrite_del_sys_tag(target_id(), tag(), dets_name())
        -> ok | {error, any()}.
dbwrite_del_sys_tag(Id, Tag, DbName) ->
    case dets:lookup(DbName, Id) of
        [Record] ->
            TagList     = Record#target.sys_tags,
            UpdatedTagList  = lists:delete(Tag, TagList),
            UpdatedRecord   = Record#target{sys_tags = UpdatedTagList},
            case dets:insert(DbName, UpdatedRecord) of
                ok ->
                    generate_event({update, UpdatedRecord});
                OtherReturn ->
                    OtherReturn
            end;
        [] ->
            {error, unknown_id}
    end.

-spec dbread_info(dets_name()) -> [#target{}].
dbread_info(DbName) ->
    dets:match(DbName, '$1').


-spec dbread_info(target_id(), dets_name()) 
        -> {ok, #target{}} | {error, unknown_id}.
dbread_info(Id, DbName) ->
    case dets:lookup(DbName, Id) of
        [Record] ->
            {ok, Record};
        [] ->
            {error, unknown_id}
    end.


-spec dbread_get_ids(dets_name()) -> [target_id()].
dbread_get_ids(DbName) ->
    dets:match(DbName, {'_', '$1', '_', '_', '_', '_', '_', '_'}).


-spec dbread_get_ip(target_id(), dets_name()) 
        -> { ok, ip_address() | undef} | {error, unknown_id}.
dbread_get_ip(Id, DbName) ->
    case dets:lookup(DbName, Id) of
        [Record] ->
            {ok, Record#target.ip};
        [] ->
            {error, unknown_id}
    end.
    

-spec dbread_get_hostname(target_id(), dets_name())
        -> { ok, hostname() | undef} | {error, unknown_id}.
dbread_get_hostname(Id, DbName) ->
    case dets:lookup(DbName, Id) of
        [Record] ->
            {ok, Record#target.hostname};
        [] ->
            {error, unknown_id}
    end.


-spec dbread_get_tags(target_id(), dets_name())
        -> { ok, [tag()]} | {error, unknown_id}.
dbread_get_tags(Id, DbName) ->
    case dets:lookup(DbName, Id) of
        [Record] ->
            {ok, Record#target.tags};
        [] ->
            {error, unknown_id}
    end.

-spec dbread_get_sys_tags(target_id(), dets_name())
        -> { ok, [tag()]} | {error, unknown_id}.
dbread_get_sys_tags(Id, DbName) ->
    case dets:lookup(DbName, Id) of
        [Record] ->
            {ok, Record#target.sys_tags};
        [] ->
            {error, unknown_id}
    end.

-spec dbread_get_property(target_id(), property_key(), dets_name())
    -> {ok, property_val() | false} | {error, unknown_id}.
dbread_get_property(Id, PropertyKey, DbName) ->
    case dets:lookup(DbName, Id) of
        [Record] ->
            Properties = Record#target.properties,
            case lists:keyfind(PropertyKey, 1, Properties) of
                {_, PropertyVal} ->
                    {ok, PropertyVal};
                false ->
                    {ok, false}
            end;
        [] ->
            {error, unknown_id}
    end.

-spec dbread_get_sys_property(target_id(), property_key(), dets_name())
    -> {ok, property_val() | false} | {error, unknown_id}.
dbread_get_sys_property(Id, PropertyKey, DbName) ->
    case dets:lookup(DbName, Id) of
        [Record] ->
            Properties = Record#target.sys_properties,
            case lists:keyfind(PropertyKey, 1, Properties) of
                {_, PropertyVal} ->
                    {ok, PropertyVal};
                false ->
                    {ok, false}
            end;
        [] ->
            {error, unknown_id}
    end.

-spec dbread_get_properties(target_id(), dets_name())
    -> [property()] | {error, unknown_id}.
dbread_get_properties(Id, DbName) ->
    case dets:lookup(DbName, Id) of
        [Record] ->
            Record#target.properties;
        [] ->
            {error, unknown_id}
    end.

-spec dbread_get_sys_properties(target_id(), dets_name())
    -> [property()] | {error, unknown_id}.
dbread_get_sys_properties(Id, DbName) ->
    case dets:lookup(DbName, Id) of
        [Record] ->
            Record#target.sys_properties;
        [] ->
            {error, unknown_id}
    end.



-spec dbread_possess_tag(target_id(), tag(), dets_name())
    -> boolean() | {error, unknown_id}.
dbread_possess_tag(Id, Tag, DbName) ->
    case dets:lookup(DbName, Id) of
        [Record] ->
            Tags = Record#target.tags,
            lists:member(Tag, Tags);
        [] ->
            {error, unknown_id}
    end.

-spec dbread_possess_sys_tag(target_id(), tag(), dets_name())
    -> boolean() | {error, unknown_id}.
dbread_possess_sys_tag(Id, Tag, DbName) ->
    case dets:lookup(DbName, Id) of
        [Record] ->
            Tags = Record#target.sys_tags,
            lists:member(Tag, Tags);
        [] ->
            {error, unknown_id}
    end.

-spec dbread_possess_property(
    property_key | property_tuple,
    target_id(),
    property_key() | property(),
    dets_name())
    -> boolean() | {error, unknown_id}.
dbread_possess_property(Type, Id, Arg, DbName) ->
    case dets:lookup(DbName, Id) of
        [Record] ->
            Properties = Record#target.properties,
            case Type of
                property_key ->
                    lists:keymember(Arg, 1, Properties);
                property_tuple ->
                    lists:member(Arg, Properties)
            end;
        [] ->
            {error, unknown_id}
    end.

-spec dbread_possess_sys_property(
    property_key | property_tuple,
    target_id(),
    property_key() | property(),
    dets_name())
    -> boolean() | {error, unknown_id}.
dbread_possess_sys_property(Type, Id, Arg, DbName) ->
    case dets:lookup(DbName, Id) of
        [Record] ->
            Properties = Record#target.sys_properties,
            case Type of
                property_key ->
                    lists:keymember(Arg, 1, Properties);
                property_tuple ->
                    lists:member(Arg, Properties)
            end;
        [] ->
            {error, unknown_id}
    end.

-spec dbread_filter([#target{}], [
    {tag, tag()} | 
    {property_key, property_key()} | 
    {property_tuple, property()}], dets_name()) -> [#target{}].
dbread_filter(all, Args, DbName) ->
    dbread_filter(info(), Args, DbName);
% TODO
dbread_filter(_Targets, Args, _DbName) ->
    _TagSearch       = lists:keytake(tag, 1, Args),
    _PropKeySearch   = lists:keytake(property_key, 1, Args),
    _PropTupleSearch = lists:keytake(property_tuple, 1, Args).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPERS                                                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec is_ip(ip_address()) -> boolean().
% TODO
is_ip(_Ip) ->
    true.

-spec valid_hostname_string(any()) -> boolean().
valid_hostname_string(Arg) ->
    Fun = fun(X) ->
        if 
            X >= 48, X =< 90    -> true; 
            X >= 97, X =< 122   -> true;
            X == 45             -> true;
            X == 95             -> true;
            true                -> false
        end
    end,
    case is_list(Arg) of
        true ->
            lists:all(Fun, Arg);
        false ->
            false
    end.

-spec generate_event({insert | update, #target{}} | {delete, target_id()}) 
        -> ok.
generate_event(Event) ->
    gen_event:notify(targets_events, Event).

-spec log(string(), list()) -> ok.
log(targets_events, Event) ->
    io:format("~p~n", [Event]);
log(A, B) ->
    io:format(A, B).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EUNIT TESTS                                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(EUNIT_DIR, "/tmp").

-spec start_stop_test() -> ok.
start_stop_test() ->
    DbFile = filename:absname_join(?EUNIT_DIR, "targets_db"),
    file:delete(DbFile),
    % start stop with no file
    ?assertMatch({ok, _},   start_link(?EUNIT_DIR)),
    ?assertMatch(true,      dets:is_dets_file(DbFile)),
    ?assertMatch(ok,        gen_server:cast(?MODULE, stop)),
    timer:sleep(100),
    % start stop with file
    ?assertMatch({ok, _},   start_link(?EUNIT_DIR)),
    ?assertMatch(true,      dets:is_dets_file(DbFile)),
    ?assertMatch(ok,        gen_server:cast(?MODULE, stop)),
    ?assertMatch(ok,        file:delete(DbFile)).

-spec valid_hostname_string_test() -> ok.
valid_hostname_string_test() ->
    ?assertMatch(false, valid_hostname_string([1,3,67])),
    ?assertMatch(false, valid_hostname_string("lkdfj_lsfjé")),
    ?assertMatch(false, valid_hostname_string("jfi786/")),
    ?assertMatch(false, valid_hostname_string([48,90, 97, 122, 45, 96])),
    ?assertMatch(false, valid_hostname_string(<<"lkj">>)),
    ?assertMatch(false, valid_hostname_string(<<91>>)),
    ?assertMatch(false, valid_hostname_string(an_atom_x)),
    ?assertMatch(true,  valid_hostname_string([48,90, 97, 122, 45, 95])),
    ?assertMatch(true,  valid_hostname_string("223-688-545")),
    ?assertMatch(true,  valid_hostname_string("AdF52dF-_")).

-spec add_remove_target_test() -> ok.
add_remove_target_test() ->
    DbFile = filename:absname_join(?EUNIT_DIR, "targets_db"),
    file:delete(DbFile),
    ?assertMatch({ok, _},   start_link(?EUNIT_DIR)),

    ?assertMatch({ok, _},   new()),
    ?assertMatch({ok, _},   new()),
    ?assertMatch([_, _],    info()),
    ?assertMatch(ok,        clear_locks()),
    ?assertMatch([],        info()),
    {ok, A} = new(),
    {ok, B} = new(),
    ?assertMatch(ok,        del_target(A)),
    ?assertMatch([_],       info()),
    ?assertMatch(ok,        del_target(B)),
    ?assertMatch([],        info()),

    ?assertMatch(ok,        gen_server:cast(?MODULE, stop)),
    ?assertMatch(ok,        file:delete(DbFile)).

-spec clear_locks_test() -> ok.
clear_locks_test() ->
    DbFile = filename:absname_join(?EUNIT_DIR, "targets_db"),
    file:delete(DbFile),
    ?assertMatch({ok, _},   start_link(?EUNIT_DIR)),
    {ok, A} = new(),
    {ok, _} = new(),
    ?assertMatch(ok,        set_tag(A, test)),
    ?assertMatch(ok,        clear_locks()),
    ?assertMatch([_],       info()),
    ?assertMatch(ok,        gen_server:cast(?MODULE, stop)),
    ?assertMatch(ok,        file:delete(DbFile)).

-spec get_set_del_possess_tag_test() -> ok.
get_set_del_possess_tag_test() ->
    DbFile = filename:absname_join(?EUNIT_DIR, "targets_db"),
    file:delete(DbFile),
    ?assertMatch({ok, _},   start_link(?EUNIT_DIR)),
    {ok, A} = new(),
    ?assertMatch(ok,        set_tag(A, test)),
    ?assertMatch(true,      possess_tag(A, test)),
    ?assertMatch(false,     possess_tag(A, test2)),
    ?assertMatch(ok,        set_tag(A, test2)),
    ?assertMatch({ok,[test2, test]},    get_tags(A)),
    ?assertMatch(ok,        del_tag(A, test)),
    ?assertMatch({ok, [test2]},         get_tags(A)),
    ?assertMatch(ok,        gen_server:cast(?MODULE, stop)),
    ?assertMatch(ok,        file:delete(DbFile)).

-spec get_set_del_possess_sys_tag_test() -> ok.
get_set_del_possess_sys_tag_test() ->
    DbFile = filename:absname_join(?EUNIT_DIR, "targets_db"),
    file:delete(DbFile),
    ?assertMatch({ok, _},   start_link(?EUNIT_DIR)),
    {ok, A} = new(),
    ?assertMatch(ok,        set_sys_tag(A, test)),
    ?assertMatch(true,      possess_sys_tag(A, test)),
    ?assertMatch(false,     possess_sys_tag(A, test2)),
    ?assertMatch(ok,        set_sys_tag(A, test2)),
    ?assertMatch({ok,[test2, test]},    get_sys_tags(A)),
    ?assertMatch(ok,        del_sys_tag(A, test)),
    ?assertMatch({ok, [test2]},         get_sys_tags(A)),
    ?assertMatch(ok,        gen_server:cast(?MODULE, stop)),
    ?assertMatch(ok,        file:delete(DbFile)).

-spec get_set_del_possess_property_test() -> ok.
get_set_del_possess_property_test() ->
    DbFile = filename:absname_join(?EUNIT_DIR, "targets_db"),
    file:delete(DbFile),
    ?assertMatch({ok, _},   start_link(?EUNIT_DIR)),
    {ok, A} = new(),
    ?assertMatch(ok,        set_property(A, {key1, val1})),
    ?assertMatch(true,      possess_property(property_key, A, key1)),
    ?assertMatch(true,      possess_property(property_tuple, A, {key1,val1})),
    ?assertMatch(ok,        set_property(A, {key2, val2})),
    ?assertMatch(ok,        set_property(A, {key2, val3})),
    ?assertMatch([{key2, val3}, {key1, val1}], get_properties(A)),
    ?assertMatch(ok,        del_property(A, key1)),
    ?assertMatch([{key2, val3}], get_properties(A)),
    ?assertMatch({ok, val3}, get_property(A, key2)),
    ?assertMatch(ok,        gen_server:cast(?MODULE, stop)),
    ?assertMatch(ok,        file:delete(DbFile)).

-spec get_set_del_possess_sys_property_test() -> ok.
get_set_del_possess_sys_property_test() ->
    DbFile = filename:absname_join(?EUNIT_DIR, "targets_db"),
    file:delete(DbFile),
    ?assertMatch({ok, _},   start_link(?EUNIT_DIR)),
    {ok, A} = new(),
    ?assertMatch(ok,        set_sys_property(A, {key1, val1})),
    ?assertMatch(true,      possess_sys_property(property_key, A, key1)),
    ?assertMatch(true,      possess_sys_property(
            property_tuple, A, {key1,val1})),
    ?assertMatch(ok,        set_sys_property(A, {key2, val2})),
    ?assertMatch(ok,        set_sys_property(A, {key2, val3})),
    ?assertMatch([{key2, val3}, {key1, val1}], get_sys_properties(A)),
    ?assertMatch(ok,        del_sys_property(A, key1)),
    ?assertMatch([{key2, val3}], get_sys_properties(A)),
    ?assertMatch({ok, val3}, get_sys_property(A, key2)),
    ?assertMatch(ok,        gen_server:cast(?MODULE, stop)),
    ?assertMatch(ok,        file:delete(DbFile)).

-spec get_set_hostname_test() -> ok.
get_set_hostname_test() ->
    DbFile = filename:absname_join(?EUNIT_DIR, "targets_db"),
    file:delete(DbFile),
    ?assertMatch({ok, _},   start_link(?EUNIT_DIR)),
    {ok, A} = new(),
    {ok, B} = new(),
    ?assertMatch({ok, undef},   get_hostname(A)),
    ?assertMatch({ok, undef},   get_hostname(B)),
    ?assertMatch(ok,        set_hostname(B, "bebelerigolo")),
    ?assertMatch(ok,        set_hostname(A, "jojolerigolo")),
    ?assertMatch({error, bad_name},        set_hostname(A, "jojo lerigolo")),
    ?assertMatch({ok, "jojolerigolo"},   get_hostname(A)),
    ?assertMatch(ok,        set_hostname(A, "othername")),
    ?assertMatch({ok, "othername"},   get_hostname(A)),
    ?assertMatch(ok,        gen_server:cast(?MODULE, stop)),
    ?assertMatch(ok,        file:delete(DbFile)).

-spec get_ids_test() -> ok.
get_ids_test() ->
    DbFile = filename:absname_join(?EUNIT_DIR, "targets_db"),
    file:delete(DbFile),
    ?assertMatch({ok, _},   start_link(?EUNIT_DIR)),

    {ok, A} = new(),
    {ok, B} = new(),
    ?assertMatch([_,_],     get_ids()),
    ?assertMatch(ok,        del_target(A)),
    ?assertMatch([_],       get_ids()),
    ?assertMatch(ok,        del_target(B)),
    ?assertMatch([],        get_ids()),
    ?assertMatch(ok,        gen_server:cast(?MODULE, stop)),
    ?assertMatch(ok,        file:delete(DbFile)).

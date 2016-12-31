%%==============================================================================
%% Sysmo NMS Network Management and Monitoring solution (http://www.sysmo.io)
%%
%% Copyright (c) 2012-2017 Sebastien Serre <ssbx@sysmo.io>
%%
%% Sysmo NMS is free software: you can redistribute it and/or modify it under
%% the terms of the GNU General Public License as published by the Free Software
%% Foundation, either version 3 of the License, or (at your option) any later
%% version.
%%
%% Sysmo NMS is distributed in the hope that it will be useful, but WITHOUT ANY
%% WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
%% A PARTICULAR PURPOSE. See the GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License along with
%% Sysmo.  If not, see <http://www.gnu.org/licenses/>.
%%==============================================================================

-module(j_server_snmpman).
-include("snmpman.hrl").
-include_lib("common_hrl/include/logs.hrl").
-behaviour(gen_server).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

% utils
-export([start_link/0]).

% API
-export([get/2, walk_tree/2, walk_table/2, discovery/4, register_element/2,
    which_elements/0, unregister_element/1, element_registered/1,
    which_usm_users/0]).

-record(state, {java_pid}).

-define(ASSERT_TIMEOUT, 5000).
-define(CALL_TIMEOUT, 10000).

% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% @private
% @doc
% Return registered usm users. Used for debug only.
% @end
which_usm_users() ->
    gen_server:call(?MODULE,
        {call_snmp4j, {which_usm_users, {}}}, ?CALL_TIMEOUT).

-spec discovery(
        Ip::string(),
        IpVersion::string(),
        Port::integer(),
        Timeout::integer()) ->
    {ok, EngineId::string()} | {error, timeout}.
% @doc
% Return the engine ID of the target as hexadecimal string.
% IpVersion can be "v4" | "v6"
% @end
discovery(Ip, IpVersion, Port, Timeout) ->
    gen_server:call(?MODULE,
       {call_snmp4j, {discovery, {Ip,IpVersion,Port,Timeout}}}, ?CALL_TIMEOUT).


-spec element_registered(ElementName::string()) -> true | false.
% @doc
% Return true if the element is registered.
% @end
element_registered(ElementName) ->
    {ok, Elements} = which_elements(),
    lists:member(ElementName, Elements).


-spec which_elements() ->
    {ok, [ElementName::string()]} | {error, Reason::term()}.
% @doc
% Return the list of registered elements.
% @end
which_elements() ->
    gen_server:call(?MODULE,
        {call_snmp4j, {which_elements, {}}}, ?CALL_TIMEOUT).



-spec unregister_element(ElementName::string()) ->
    ok | {error, Reason::term()}.
% @doc
% Remove the specified element.
% @end
unregister_element(ElementName) ->
    gen_server:call(?MODULE, {call_snmp4j,
                   {unregister_element, {ElementName}}}, ?CALL_TIMEOUT).



-spec register_element(
        TargetName::string(),
        TargetConf::[]) ->
    ok | {error, Reason::term()}.
% @doc
% Register a new element.
%
% TargetName is an arbitrary string() wich identify the target.
%
% TargetConf is a list of property:
%
% <b>MANDATORY</b>:
%
% {host, Host} when Host can be ipv4/6 or hostname
%
% {snmp_version, V} when V = "1" | "2c" | "3",
%
% <b>OPTIONAL</b>:
%
% {security_level, SecLevel} when SecLevel =
%   "authPriv" | "authNoPriv" | "noAuthNoPriv", default to "noAuthNoPriv",
%
% {port, Port} when Port is an integer. Default to 161.
%
% {retries, Retries} when Retries is an integer. Number of retries when timeout
% occur. Default to 1.
%
% {timeout, Timeout} when Timeout is an integer. Set the time in milliseconds
% after wich the manager will return {error, timeout}. Default is 2000 (2 seconds).
%
% {community, Community} when community is a string. Default to public.
%
% {priv_proto, PrivProto} when PrivProto = "AES" | "AES192" | "AES356" |
% "DES" | "3DES" (default to "AES"),
%
% {auto_proto, AuthProto} when AuthProto = "MD5" | "SHA" (default to "SHA")
%
% {priv_key, Key} and {auth_key, Key} when key must be a string of minimum
% 8 characters. Defaults are the string "undefined".
%
% <b>MANDATORY</b> if version = "3":
% {security_name, Name} when Name is non empty string. Name is the name of the USM
% user to use for the agent.
%
% @end
register_element(ElementName, ElementConf) ->
    ElementDef = build_conf(ElementName,ElementConf),
    ?LOG_INFO("Register:", ElementDef),
    gen_server:call(?MODULE,
                    {call_snmp4j, {register_element, ElementDef}}, ?CALL_TIMEOUT).



-spec walk_table(Target::string(), Oids::[Oid::string()]) ->
        {ok, Reply::term()} |
        {error, Reason::term()} |
        {report, Report::string}.
% @doc
% Walk a table and return the rows specified in the Oids in the form of a
% tuple.
%
% Example returning table rows from the mib2 <b>ifTable</b>. We want
% to get <b>ifName</b>, <b>ifTypes</b> and <b>ifSpeed</b> for all interfaces:
% ```
% walk_table("myTarget", [
%   "1.3.6.1.2.1.2.2.1.2",
%   "1.3.6.1.2.1.2.2.1.3",
%   "1.3.6.1.2.1.2.2.1.5"
%   ]).'''
% ```
% {ok,[{varbinds,[{table_row,"sis0",6,100000000},
%               {table_row,"lo0",24,0},
%               {table_row,"pflog0",1,0},
%               {table_row,"enc0",1,0},
%               {table_row,"bridge0",209,0},
%               {table_row,"sis2",6,0},
%               {table_row,"sis1",6,100000000}]}]}'''
%
% @end
walk_table(Target, Oids) ->
    Cfg = {Target, Oids},
    gen_server:call(?MODULE,
                    {call_snmp4j, {walk_table, Cfg}}, ?CALL_TIMEOUT).


-spec walk_tree(Target::string(), Oid::string()) ->
        {ok, Reply::term()} |
        {error, Reason::term()} |
        {report, Report::string}.
% @doc
% Walk the specified Oid on the target Target.
% @end
walk_tree(Target, Oid) ->
    Cfg = {Target, Oid},
    gen_server:call(?MODULE,
                    {call_snmp4j, {walk_tree, Cfg}}, ?CALL_TIMEOUT).




-spec get(Target::string(), [Oid::string()]) ->
        {ok, Reply::term()} |
        {error, Reason::term()} |
        {report, Report::string}.
% @doc
% Get the listed Oids from the target Target.
% @end
get(Target, Oids) ->
    % OID validity is make by the snmp4j lib.
    Cfg = {Target, Oids},
    gen_server:call(?MODULE,
                    {call_snmp4j, {get, Cfg}}, ?CALL_TIMEOUT).



% GEN_SERVER
% @private
init([]) ->
    JavaPid = j_server:get_pid(snmp4j),
    ?LOG_INFO("success pid", JavaPid),
    {ok, #state{java_pid=JavaPid}}.

% CALL
% @private
handle_call({call_snmp4j, {Command, Payload}}, From, S) ->
    S#state.java_pid ! {Command, From, Payload},
    {noreply, S}.

% @private
handle_cast(_Cast,S) ->
    ?LOG_INFO("received handle cast:", _Cast),
    {noreply, S}.


% INFO
% @private
handle_info(stop, S) ->
    ?LOG_INFO("Received stop"),
    {noreply, S};

handle_info({reply, From, Reply}, S) ->
    gen_server:reply(From, Reply),
    {noreply, S};

handle_info(_I, S) ->
    ?LOG_INFO("received handle info:", _I),
    {noreply, S}.


% TERMINATE
% @private
terminate(_,_) ->
    ok.


% CHANGE
% @private
code_change(_,S,_) ->
    {ok, S}.


% PRIVATE

build_conf(ElementName, ElementConf) ->
    PrivProto = proplists:get_value(priv_proto, ElementConf, "AES"),
    PrivKey   = proplists:get_value(priv_key,   ElementConf, "undefined"),
    AuthProto = proplists:get_value(auth_proto, ElementConf, "SHA"),
    AuthKey   = proplists:get_value(auth_key,   ElementConf, "undefined"),
    Host      = proplists:get_value(host,       ElementConf, "undefined"),
    Port      = proplists:get_value(port,       ElementConf, "161"),
    Version   = proplists:get_value(snmp_version, ElementConf, ""),
    SecLevel  = proplists:get_value(security_level, ElementConf, "noAuthNoPriv"),
    Retries   = proplists:get_value(retries,    ElementConf, "1"),
    Timeout   = proplists:get_value(timeout,    ElementConf, "2000"),
    SecName   = proplists:get_value(security_name, ElementConf, "undefined"),
    Community = proplists:get_value(community,     ElementConf, "public"),
    #element_def{
        name            = ElementName,
        host            = Host,
        port            = Port,
        snmp_version    = Version,
        sec_level       = SecLevel,
        retries         = Retries,
        timeout         = Timeout,
        sec_name        = SecName,
        community       = Community,
        auth_proto      = AuthProto,
        auth_key        = AuthKey,
        priv_proto      = PrivProto,
        priv_key        = PrivKey
    }.

% Copyright (C) 2014, Sebastien Serre <sserre.bx@gmail.com>
% 
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
% 
% The above copyright notice and this permission notice shall be included in all
% copies or substantial portions of the Software.
% 
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
% THE SOFTWARE.

-module(snmpman).
-include("include/snmpman.hrl").
-behaviour(gen_server).

% gen_server
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    start_link/0,
    assert_init/0
]).


% API
-export([
    get/2,
    walk_tree/2,
    walk_table/2,

    discovery/4,

    register_element/2,
    which_elements/0,
    unregister_element/1,

    element_registered/1,

    which_usm_users/0
]).

-record(state, {
    snmp4j_pid      = undefined,
    replies_waiting = [],
    assert_init     = undefined
}).

-define(ASSERT_TIMEOUT, 5000).

% @private
start_link() ->
    Ret = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    ok = assert_init(),
    Ret.

% @private
% @doc
% Called by start_link to ensure initialisation before returning.
% @end
assert_init() ->
    gen_server:call(?MODULE, assert_init, ?ASSERT_TIMEOUT).


% @private
% @doc
% Return registered usm users. Used for debug only.
% @end
which_usm_users() ->
    gen_server:call(?MODULE,
        {call_snmp4j, {which_usm_users, {}}}, infinity).

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
    case snmpman_guard:validate_discovery_conf(Ip, IpVersion, Port, Timeout) of
        true ->
            gen_server:call(?MODULE, 
                {call_snmp4j, {discovery, {Ip,IpVersion,Port,Timeout}}}, infinity);
        {false, Reason} ->
            {error, Reason}
    end.


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
        {call_snmp4j, {which_elements, {}}}, infinity).



-spec unregister_element(ElementName::string()) ->
    ok | {error, Reason::term()}.
% @doc
% Remove the specified element.
% @end
unregister_element(ElementName) ->
    gen_server:call(?MODULE,
        {call_snmp4j, {unregister_element, {ElementName}}}, infinity).



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
    io:format("register: ~p~n", [ElementDef]),
    case snmpman_guard:validate_register_conf(ElementDef) of
        true ->
            gen_server:call(?MODULE,
                {call_snmp4j, {register_element, ElementDef}}, infinity);
        Error ->
            Error
    end.



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
    case snmpman_guard:validate_oids(Oids) of
        true ->
            gen_server:call(?MODULE, {call_snmp4j, {walk_table, Cfg}}, infinity);
        false ->
            {error, "Bad OID value"}
    end.


-spec walk_tree(Target::string(), Oid::string()) -> 
        {ok, Reply::term()} | 
        {error, Reason::term()} | 
        {report, Report::string}.
% @doc
% Walk the specified Oid on the target Target.
% @end
walk_tree(Target, Oid) ->
    Cfg = {Target, Oid},
    case snmpman_guard:validate_oids([Oid]) of
        true ->
            gen_server:call(?MODULE, {call_snmp4j, {walk_tree, Cfg}}, infinity);
        false ->
            {error, "Bad OID value"}
    end.




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
    case snmpman_guard:validate_oids(Oids) of
        true ->
            gen_server:call(?MODULE, {call_snmp4j, {get, Cfg}}, infinity);
        false ->
            {error, "Bad OID value"}
    end.



% GEN_SERVER
% @private
init([]) ->
    process_flag(trap_exit, true),
    gen_server:cast(?MODULE, boot),
    {ok, #state{}}.

% CALL 
% @private
handle_call(assert_init, F, #state{snmp4j_pid = undefined} = S) ->
    {noreply, S#state{assert_init = F}};
handle_call(assert_init, _F, S) ->
    {reply, ok, S};

handle_call({call_snmp4j, {Command, Payload}}, From, 
        #state{snmp4j_pid = Snmp4j, replies_waiting = RWait} = S) ->
    Snmp4j ! {Command, From, Payload},
    {noreply, S#state{replies_waiting = [From|RWait]}}.


% CAST
% @private
handle_cast(boot, S) ->
    boot(),
    {noreply, S};

handle_cast(_,S) ->
    {noreply, S}.


% INFO
% @private
handle_info({Pid, snmp4j_running}, S) ->
    io:format("receive init~n"),
    case S#state.assert_init of
        undefined -> 
            ok;
        F ->
            gen_server:reply(F, ok)
    end,
    erlang:link(Pid),
    {noreply, 
        S#state{
            snmp4j_pid      = Pid,
            replies_waiting = [],
            assert_init     = undefined
        }
    };

% @private
handle_info(stop, S) ->
    io:format("received stop~n"),
    {noreply, S};

handle_info({reply, From, Reply}, #state{replies_waiting = RWait} = S) ->
    gen_server:reply(From, Reply),
    {noreply, 
        S#state{
            replies_waiting = lists:delete(From, RWait)
        }
    };

handle_info({'EXIT', Pid, Reason}, #state{snmp4j_pid = Pid} = S) ->
    io:format("snmp4j EXIT with reason: ~p~n", [Reason]),
    {stop, Reason, S};

handle_info(_I, S) ->
    io:format("received handle info: ~p~n", [_I]),
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
boot() ->
    Prefix   = sysmo:get_java_bin_prefix(),
    Relative = string:concat("snmpman/bin/snmpman", Prefix),
    Cmd = filename:join(filename:absname(sysmo:get_java_dir()),Relative),
    WorkDir = filename:absname(""),
    Node = sysmo:get_node_name(),
    erlang:open_port({spawn_executable, Cmd},
                     [{args,[WorkDir, Node]}, stderr_to_stdout]).

build_conf(ElementName, ElementConf) ->
    PrivProto = proplists:get_value(priv_proto, ElementConf, "AES"),
    PrivKey   = proplists:get_value(priv_key,   ElementConf, "undefined"),
    AuthProto = proplists:get_value(auth_proto, ElementConf, "SHA"),
    AuthKey   = proplists:get_value(auth_key,   ElementConf, "undefined"),
    Host      = proplists:get_value(host,       ElementConf, "undefined"),
    Port      = proplists:get_value(port,       ElementConf, 161),
    Version   = proplists:get_value(snmp_version, ElementConf, ""),
    SecLevel  = proplists:get_value(security_level, ElementConf, "noAuthNoPriv"),
    Retries   = proplists:get_value(retries,    ElementConf, 1),
    Timeout   = proplists:get_value(timeout,    ElementConf, 2000),
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

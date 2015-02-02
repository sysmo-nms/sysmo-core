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

-module(nchecks).
-include("include/nchecks.hrl").
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
    tcp/1,
    icmp/1
]).

-record(state, {
    nchecks_pid     = undefined,
    replies_waiting = [],
    java_com        = "",
    assert_init     = undefined
}).

-define(ASSERT_TIMEOUT, 60000).

-spec tcp(ArgList::[{PropertyConf::string(), Property::any}]) ->
    {ok, Reply::#nchecks_reply{}} | {error, Reason::any()}.
% @doc
% Execute a TCP check. Arglist is a property list of configuration
% directives.
%
% <b>MANDATORY</b>
%
% {"host", Host} when Host is a string representing a hostname, an ipv4 address,
% or an ipv6 address.
%
% {"port", Port} when port is an integer representing the port.
%
% <b>OPTIONAL</b>
%
% {"ms_warning", Value} when Value is an integer representing milliseconds.
% If the connection time exceed Value, the return will have status of
% "WARNING". Default is 500.
%
% {"ms_critical", Value} when Value is an integer representing milliseconds.
% If the connection time exceed Value, the return will have status of
% "CRITICAL". Default is 2500.
%
% {"ms_timeout", Value} when Value is an integer representing milliseconds.
% Abort connection if time exceed Value. Default is 5000.
%
% {"refuse", Value} when Value is one of "OK"|"WARNING"|"CRITICAL". If the
% connexion is refused or fail, set the return status
% to Value. May be set to "OK" to assert that a service is down. Default
% is "CRITICAL". Use in conjontion with {accept, _}. Default is "OK". Note
% that when Value is not "CRITICAL", {ms_warning,_} and {ms_critical,_} are
% ignored.
%
% {"accept", Value} when Value is one of "OK"|"WARNING"|"CRITICAL". If the
% connexion success set thre return status to Value. May be set to "CRITICAL"
% to alert that a service is up. Use in conjonction with
% {"refuse",_}. Default is "OK". Note that when Value is not "OK",
% {"ms_warning",_} and {ms_critical,_} are ignored.
%
% @end
tcp(ArgList) ->
    gen_server:call(?MODULE, {call_nchecks, {check_TCP, {ArgList}}}, infinity).



-spec icmp(ArgList::[{PropertyConf::string(), Property::any}]) ->
    {ok, Reply::#nchecks_reply{}} | {error, Reason::any()}.
% @doc
% Execute a ICMP check. Arglist is a property list of configuration
% directives.
%
% <b>MANDATORY</b>
%
% {"host", Host} when Host is a string representing a hostname, an ipv4 address,
% or an ipv6 address.
%
% <b>OPTIONAL</b>
%
% {"pkts_number", Value} when Value is an integer. Send Value number of packets.
% Default is 5.
%
% {"pkts_size", Value} when Value is an integer. Set the packets size to Value.
% Default is 56.
%
% {"pl_warning", Value} when Value is an integer between 0 and 100 representing
% percents. If packet lost exceed Value percent, set reply status to
% "WARNING". Default is 50.
%
% {"pl_critical", Value} when Value is an integer between 0 and 100 representing
% percents. If packet lost exceed Value percent, set reply status to
% "CRITICAL". Default is 100.
%
% {"ms_warning", Value} when Value is an integer representing milliseconds.
% If the average reply time exceed Value, the return will have status of
% "WARNING". Default is 200.
%
% {"ms_critical", Value} when Value is an integer representing milliseconds.
% If average reply time exceed Value, the return will have status of
% "CRITICAL". Default is 2500.
%
% {"ms_timeout", Value} when Value is an integer representing milliseconds.
% Abort if reply time exceed Value. Default is 5000.
%
% {"ms_interval", Value} when Value is an integer. Send heach packets with an
% interval Value milliseconds. Default is 100.
%
% {"useIpv6", Value} when Valie is "true" or "false". Will return DOWN status
% if the Host argument can not be translated to ip version 6. Default is false.
%
% @end
icmp(ArgList) ->
    gen_server:call(?MODULE, {call_nchecks, {check_ICMP, {ArgList}}}, infinity).


% @private
% @doc
% Called by start_link to ensure initialisation before returning.
% @end
assert_init() ->
    gen_server:call(?MODULE, assert_init, ?ASSERT_TIMEOUT).

% @private
start_link() ->
    Ret = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    ok = assert_init(),
    Ret.

% GEN_SERVER
% @private
init([]) ->
    process_flag(trap_exit, true),
    {ok, JavaConf}  = application:get_env(nchecks, java_node),
    JavaCommand     = proplists:get_value(java_com, JavaConf),
    gen_server:cast(?MODULE, boot),
    {ok, #state{java_com=JavaCommand}}.

% CALL 
% @private
handle_call(assert_init, F, #state{nchecks_pid = undefined} = S) ->
    {noreply, S#state{assert_init = F}};
handle_call(assert_init, _F, S) ->
    {reply, ok, S};

handle_call({call_nchecks, {Command, Payload}}, From, 
        #state{nchecks_pid = NChecks, replies_waiting = RWait} = S) ->
    NChecks ! {Command, From, Payload},
    {noreply, S#state{replies_waiting = [From|RWait]}}.


% CAST
% @private
handle_cast(boot, #state{java_com = JavaCommand} = S) ->
    boot(JavaCommand),
    {noreply, S};

handle_cast(_,S) ->
    {noreply, S}.


% INFO
% @private
handle_info({Pid, nchecks_running}, S) ->
    io:format("receive init~n"),
    case S#state.assert_init of
        undefined -> 
            ok;
        F ->
            gen_server:reply(F, ok)
    end,
    erlang:link(Pid),
    Pid ! {init, {}, {empty_init}},
    {noreply, 
        S#state{
            nchecks_pid      = Pid,
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

handle_info({'EXIT', Pid, Reason}, #state{nchecks_pid = Pid} = S) ->
    io:format("nchecks EXIT with reason: ~p~n", [Reason]),
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
boot(JavaCommand) ->
    os:cmd(JavaCommand).

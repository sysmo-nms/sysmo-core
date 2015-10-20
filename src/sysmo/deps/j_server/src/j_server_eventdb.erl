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

-module(j_server_eventdb).
-include("eventdb.hrl").
-include_lib("common_hrl/include/logs.hrl").
-behaviour(gen_server).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-export([start_link/0]).

-export([notify/1, dump_latest_events/1, dump_probe_events/2]).

-record(state, {java_pid}).

-define(ASSERT_TIMEOUT, 5000).
-define(CALL_TIMEOUT,   10000).

% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec notify(Notif::#notification{}) -> ok.
notify(Notif) ->
    gen_server:cast(?MODULE, {notify, Notif}).


-spec dump_latest_events(DumpDir::string()) ->
        {ok, FileName::string()} | {error, Error::string()}.
% @doc
% return two latests months of events in json file.
% @end
dump_latest_events(DumpDir) ->
    gen_server:call(?MODULE,
        {call_eventdb, {dump_latest_events, DumpDir}}, ?CALL_TIMEOUT).


-spec dump_probe_events(DumpDir::string(), Probe::string()) ->
        {ok, FileName::string()} | {error, Error::string()}.
% @doc
% return all events for probe Probe in json file.
% @end
dump_probe_events(DumpDir, Probe) ->
    gen_server:call(?MODULE,
        {call_eventdb, {dump_probe_events, {Probe, DumpDir}}}, ?CALL_TIMEOUT).


% GEN_SERVER
% @private
init([]) ->
    JavaPid = j_server:get_pid(eventdb),
    ?LOG_INFO("success pid", JavaPid),
    {ok, #state{java_pid=JavaPid}}.

% @private
handle_call({call_eventdb, {Command, Payload}}, From, S) ->
    S#state.java_pid ! {Command, From, Payload},
    {noreply, S}.

% @private
handle_cast(Msg, #state{java_pid=Pid} = S) ->
    Pid ! Msg,
    {noreply, S};

handle_cast(_C,S) ->
    ?LOG_INFO("Unknown cast", _C),
    {noreply, S}.

% @private
handle_info(stop, S) ->
    ?LOG_INFO("Received stop"),
    {noreply, S};

handle_info({reply, From, Reply}, S) ->
    gen_server:reply(From, Reply),
    {noreply, S};

handle_info({'EXIT', Pid, Reason}, #state{java_pid = Pid} = S) ->
    ?LOG_WARNING("eventdb EXIT with reason:", Reason),
    {stop, Reason, S};

handle_info(_I, S) ->
    ?LOG_INFO("received handle info:", _I),
    {noreply, S}.

% @private
terminate(_,_) -> ok.

% @private
code_change(_,S,_) -> {ok, S}.

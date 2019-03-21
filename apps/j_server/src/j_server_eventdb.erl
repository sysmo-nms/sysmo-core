%%==============================================================================
%% Sysmo NMS Network Management and Monitoring solution (https://sysmo-nms.github.io)
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

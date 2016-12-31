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

-module(j_server_errd4j).
-include("errd4j.hrl").
-include_lib("common_hrl/include/logs.hrl").
-behaviour(gen_server).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-export([start_link/0]).

-export([create/3, create/4, multi_create/1, update/3, multi_update/1]).
% MAYBE export
%update_fetch/0,
%updates_fetch/0,

-record(state, {java_pid}).

-define(ASSERT_TIMEOUT, 5000).
-define(CALL_TIMEOUT,   10000).


-spec multi_update(Updates::[tuple()]) -> ok.
% @doc
% Send multiple update/3 in one call. Arg is a list of tuple.
% Each tuple have the same content as update/3.
% @end
multi_update([]) -> ok;
multi_update(Updates) ->
    gen_server:call(?MODULE,
        {call_errd4j, {multi_update, {Updates}}}, ?CALL_TIMEOUT).

-spec update(File::string(), Updates::{string(), integer()},
        Timestamp::integer()) -> ok.
% @doc
% Execute an update to a single rrdfile.
% example: Updates = [{"MaxRoundTrip", 3000}, {"MinRoundTrip", 399}],
% @end
update(File, Updates, Timestamp) ->
    Args = {File, Updates, Timestamp},
    gen_server:call(?MODULE, {call_errd4j, {update, Args}}, ?CALL_TIMEOUT).


-spec multi_create(Args::list()) -> ok.
% @doc
% Send multiple create command in one call. Args is a list of tuples with the
% same elements as in create/4 call.
% Example = [{"jojo.rrd",300,
%   [{"speed", "COUNTER", 600, 0, 'Nan'}],"default"}...]
% @end
multi_create(Args) ->
    gen_server:call(?MODULE,
        {call_errd4j, {multi_create, {Args}}}, ?CALL_TIMEOUT).


-spec create(File::string(), Step::integer(), DDs::[]) -> ok.
% @doc
% Same as create(File,Step,DDs,"default")
% @end
create(File,Step, DSs) ->
    create(File,Step,DSs,"default").

-spec create(File::string(), Step::integer(), DSs::[], RRAType::string()) -> ok.
% @doc
% Where RRAType is "default" or "precise"
% File: a string represnting the rrd file created
% Step: an integer
% DSs: [{DsName::string(), DsType::string(), HeartBeat::integer(), Min::integer(), Max::integer()}]
%   example: [{"speed", "COUNTER", 600, 0, 'Nan'}]
% @end
create(File, Step, DSs, RRAType) ->
    Args = {File,Step,DSs,RRAType},
    gen_server:call(?MODULE, {call_errd4j, {create, Args}}).



% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


% GEN_SERVER
% @private
init([]) ->
    JavaPid = j_server:get_pid(rrd4j),
    ?LOG_INFO("success pid", JavaPid),
    {ok, #state{java_pid=JavaPid}}.

% @private
handle_call({call_errd4j, {Command, Payload}}, From, S) ->
    S#state.java_pid ! {Command, From, Payload},
    {noreply, S}.

% @private
handle_cast(_,S) ->
    {noreply, S}.

% @private
handle_info(stop, S) ->
    ?LOG_INFO("Received stop"),
    {noreply, S};

handle_info({reply, From, Reply}, S) ->
    gen_server:reply(From, Reply),
    {noreply, S};

handle_info({'EXIT', Pid, Reason}, #state{java_pid = Pid} = S) ->
    ?LOG_WARNING("rrd4j EXIT with reason:", Reason),
    {stop, Reason, S};

handle_info(_I, S) ->
    ?LOG_INFO("received handle info:", _I),
    {noreply, S}.

% @private
terminate(_,_) -> ok.

% @private
code_change(_,S,_) -> {ok, S}.

-module(tracker_events).
-include("../tracker/include/tracker.hrl").
-behaviour(gen_server).

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
    init/2,
    log/2,
    dump/1
]).

%-record(probe_event, {
    %insert_timestamp,
    %ack_timestamp,
    %other
%}).


init(Conf, ProbeServerState) ->
    gen_server:call(?MODULE, {init, Conf, ProbeServerState}).

log(ProbeServerState, #probe_return{is_event = true} = ProbeReturn) ->
    gen_server:cast(?MODULE, {log, ProbeServerState, ProbeReturn});
log(_ProbeServerState, _ProbeReturn) ->
    ok.

dump(ProbeServerState) ->
    gen_server:call(?MODULE, {dump, ProbeServerState}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, []}.


%  
handle_call({init, _Conf, 
        #probe_server_state{probe = _Probe} = ProbeServerState
    }, _F, S) ->
    %mnesia:create_table(Probe#probe.name, []),
        %[
            %{attributes, record_info(fields, probe_event)},
            %{disc_only_copies, [node()]}
        %]
    %),

    %Rep = mnesia:table_info(Probe#probe.name, all),
    %Rep = mnesia:info(),
    ?LOG({"init_tracker_events ---------------------------->"}),
    {reply, {ok, ProbeServerState}, S};

handle_call({dump, _ProbeServerState}, _F, S) ->
    ?LOG("dump_tracker_events"),
    {reply, ignore, S};

handle_call(_R, _F, S) ->
    {noreply, S}.

% 
handle_cast({log, _ProbeServerState, _ProbeReturn}, S) ->
    ?LOG("log_tracker_events"),
    {noreply, S};

handle_cast(_,S) ->
    {noreply, S}.

%
handle_info(_, S) ->
    {noreply, S}.

%
terminate(_,_) ->
    ok.

%
code_change(_,S,_) ->
    {ok, S}.

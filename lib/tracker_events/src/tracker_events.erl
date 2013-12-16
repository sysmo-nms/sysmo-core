-module(tracker_events).
-include("../tracker/include/tracker.hrl").
-behaviour(gen_server).
-behaviour(beha_tracker_logger).

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

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% 
init(Conf, ProbeServerState) ->
    gen_server:call(?MODULE, {init, Conf, ProbeServerState}).

log(ProbeServerState, #probe_return{is_event = true} = ProbeReturn) ->
    gen_server:cast(?MODULE, {log, ProbeServerState, ProbeReturn});
log(_ProbeServerState, _ProbeReturn) ->
    ok.

dump(ProbeServerState) ->
    gen_server:call(?MODULE, {dump, ProbeServerState}).


init([]) ->
    % TODO a single channel for the server status and events
    Rep = mnesia:table_info(schema, tables),
    lists:foreach(fun(X) ->
        insert_cold_start(X)
    end, Rep),
    % read config, some event will not require acknowledgements or/and
    % send notifications.
    {ok, []}.


%  
handle_call({init, _Conf, 
        #probe_server_state{probe = Probe} = ProbeServerState
    }, _F, S) ->
    create_table(Probe#probe.name),
    {reply, {ok, ProbeServerState}, S};

handle_call({dump, _ProbeServerState}, _F, S) ->
    ?LOG("dump_tracker_events"),
    {reply, ignore, S};

handle_call(_R, _F, S) ->
    {noreply, S}.

% 
handle_cast({log, #probe_server_state{probe = #probe{name = TableName}}, 
        ProbeReturn}, S) ->
    insert_record(TableName, ProbeReturn),
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

%
% @private
% 
create_table(TableName) ->
    Rep = lists:member(TableName, mnesia:table_info(schema, tables)),
    create_table(TableName, Rep).
create_table(_,         true) -> ok;
create_table(TableName, false) ->
    mnesia:create_table(TableName,
        [
            {access_mode, read_write},
            {attributes, record_info(fields, probe_event)},
            {disc_copies, [node()]},
            {record_name, probe_event},
            {type, set}
        ]
    ).

insert_record(TableName,#probe_return{status = Status, original_reply = String,
            timestamp = Ts}) ->
    F = fun() ->
        Id = mnesia:table_info(TableName, size),
        mnesia:write(TableName, 
            #probe_event{
                id          = Id,
                insert_ts   = Ts,
                ack_ts      = none,
                status      = Status,
                textual     = String,
                ack_needed  = true
            }, write
        )
    end,
    mnesia:transaction(F).

insert_raw_record(
        TableName, 
        #probe_return{status=Status, original_reply=String, timestamp=Ts},
        AckNeeded
    ) ->
    F = fun() ->
        Id = mnesia:table_info(TableName, size),
        mnesia:write(TableName, 
            #probe_event{
                id          = Id,
                insert_ts   = Ts,
                ack_ts      = none,
                status      = Status,
                textual     = String,
                ack_needed  = AckNeeded
            }, write
        )
    end,
    mnesia:transaction(F).

insert_cold_start(TableName) ->
    Record = mnesia:table_info(TableName, record_name),
    insert_cold_start(TableName, Record).
insert_cold_start(TableName,    probe_event) ->
    {_, MicroSec} = sys_timestamp(),
    insert_raw_record(TableName, 
        #probe_return{
            status          = 'UNKNOWN', 
            original_reply  = "Server cold start",
            timestamp       = MicroSec}, false);
insert_cold_start(_TableName,   _Record) ->
    ok.

sys_timestamp() ->
    {Meg, Sec, Micro} = os:timestamp(),
    Seconds      = Meg * 1000000 + Sec,
    Microseconds = Seconds * 1000000 + Micro,
    {Seconds, Microseconds}.

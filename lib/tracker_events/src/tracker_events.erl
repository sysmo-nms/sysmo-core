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
                acknowledged_ts = none,
                status      = Status,
                textual     = String,
                ack_needed  = true
            }, write
        )
    end,
    mnesia:transaction(F).

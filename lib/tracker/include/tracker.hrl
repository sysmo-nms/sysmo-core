-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/file.hrl").
-include("../supercast/include/supercast.hrl").

-define(LOG(X), io:format("{~p, ~p}: DEBUG: ~p~n", [?MODULE, ?LINE, X])).

-record(inspector, {
    module,
    conf
}).

-record(logger, {
    module,
    conf
}).

-record(nagios_plugin_conf, {
    executable  = undefined             :: string(),
    args        = []                    :: [{any(), any()}],
    eval_perfs  = false                 :: false | true
}).

-record(probe_return, {
    status          = 'UNKNOWN' :: 'OK' | 'UNKNOWN' | 'WARNING' | 'CRITICAL',
    original_reply  = undefined :: string(),
    timestamp       = undefined :: integer(),
    key_vals        = []        :: [{string(), any()}],
    is_event        = false     :: true | false
}).

-record(probe, {
    id                  = undefined     :: integer(), % unique in a target
    pid                 = undefined     :: undefined | pid(),
    name                = undefined     :: string(),
    permissions         = #perm_conf{}  :: #perm_conf{},
    tracker_probe_mod   = undefined     :: undefined | module(),
    tracker_probe_conf  = undefined     :: [any()],
    status              = 'UNKNOWN'     :: 'UNKNOWN' | atom(),
    timeout             = 5             :: integer(),
    step                = 60            :: integer(),
    inspectors          = []            :: [#inspector{}],
    loggers             = []            :: [#logger{}],
    properties          = []            :: [{string(), any()}],
    active              = true          :: true | false
}).

-record(target, {
    id          = undefined     :: atom(),
    global_perm = #perm_conf{
        read        =   ["admin"],
        write       =   ["admin"]
    },
    properties  = [
        {ip,            undefined},
        {hostname,      undefined},
        {sysname,       undefined},
        {snmp_conf,     undefined}
    ]        :: [{any(), any()}],
    probes      = [] :: [#probe{}],
    directory   = ""
}).

-record(probe_server_state, {
    target,
    probe,
    inspectors_state    = [],
    loggers_state       = [],
    probes_state        = []
}).

-record(snmp_conf, {
    agent_name  = none          :: string(),
    ip          = none          :: string(),
    port        = 161           :: integer(),
    version     = v2            :: v2 | v3,
    seclevel    = none          :: none | auth | enc,
    community   = "none"        :: string(),
    authkey     = none          :: none | string(),
    authalgo    = none          :: none | 'hmac-md5' | 'hmac-sha1',
    enckey      = none          :: none | string(),
    encalgo     = none          :: none | des | aes,
    oids        = []            :: [any()]
}).


% RRD related. The max line accpeted by rrdtool is reached with
% a 48 ports switch. It is why we need to break rrd databases
% in little parts.
-record(rrd_binds, {
    name    = ""                :: string(),
    macro   = ""                :: string()
}).

-record(rrd_config, {
    file    = ""                :: string(),
    create  = ""                :: string(),
    update  = ""                :: string(),
    graphs  = []                :: [string()],
    binds   = []                :: [{string(), string()}], % {replacement, macro}
    update_regexps = none       :: [any()],                % {key, re}
    file_path = none            :: string()
}).

% tracker_events
-record(probe_event, {
    id,
    insert_ts,
    ack_ts,
    status,
    textual,
    ack_needed,
    group_owner,
    user_owner
}).

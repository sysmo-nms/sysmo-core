-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/file.hrl").
-include("../supercast/include/supercast.hrl").

-define(LOG(X), io:format("{~w, ~w}: DEBUG: ~p~n", [?MODULE, ?LINE, X])).

-record(inspector, {
    module,
    conf
}).

-record(logger, {
    module,
    conf
}).

-record(ncheck_probe_conf, {
    executable  = undefined     :: string(),
    args        = []            :: [{any(), any()}],
    eval_perfs  = false         :: false | true
}).

-record(nagios_probe_conf, {
    executable  = undefined             :: string(),
    args        = []                    :: [{any(), any()}],
    eval_perfs  = false                 :: false | true
}).

-record(snmp_probe_conf, {
    port        = 161           :: integer(),
    version     = "v2"          :: string(), % "1" | "2c" | "3"
    seclevel    = "noAuthNoPriv" :: string(), % "authPriv", "authNoPriv", "noAuthNoPriv"
    community   = "none"        :: string(),
    usm_user    = "undefined"   :: string(),
    authkey     = none          :: none | string(),
    authproto   = "SHA"          :: string(),
    privkey     = none          :: none | string(),
    privproto   = "AES"          :: string(),
    oids        = []            :: [any()],
    engine_id   = "undefined"   :: string(),
    method      = get           :: get | {walk, any()},
    retries     = 1             :: integer()
}).



-record(probe_return, {
    status          = 'UNKNOWN' :: 'OK' | 'UNKNOWN' | 'WARNING' | 'CRITICAL',
    original_reply  = undefined :: string(),
    timestamp       = undefined :: integer(),
    key_vals        = []        :: [{string(), any()}],
    is_event        = false     :: true | false % used by monitor_logger_events app
}).

-record(probe, {
    id                  = undefined     :: integer(), % unique in a target
    pid                 = undefined     :: undefined | pid(),
    name                = undefined     :: atom(),
    description         = ""            :: string(),
    info                = ""            :: string(),
    permissions         = #perm_conf{}  :: #perm_conf{},
    monitor_probe_mod   = undefined     :: undefined | module(),
    monitor_probe_conf  = undefined     :: [any()],
    status              = 'UNKNOWN'     :: 'UNKNOWN' | atom(),
    timeout             = 5             :: integer(), % seconds
    step                = 300           :: integer(), % seconds
    inspectors          = []            :: [#inspector{}],
    loggers             = []            :: [#logger{}],
    parents             = []            :: [atom()],
    properties          = []            :: [{string(), any()}],
    forward_properties  = []            :: [string()],
    active              = true          :: true | false
}).

-record(target, {
    id          = undefined     :: atom(),
    ip          = undefined     :: string(),
    ip_version  = undefined     :: string(),
    global_perm = #perm_conf{
        read        =   ["admin"],
        write       =   ["admin"]
    },
    properties  = []            :: [{any(), any()}],
    probes      = []            :: [#probe{}],
    directory   = ""            :: string()
}).

-record(probe_set, {
    name,
    perm  = undefined   :: #perm_conf{},    % who is allowed to generate this
                                            % set.
    probe = []          :: [#probe{}]
}).

-record(ps_state, {
    target,
    name,
    probe,
    step,
    timeout,
    tref,
    last_check,
    check_state         = stopped   :: ready | running | stopped,
    check_flag          = normal    :: normal | force | random,
    nego_parents        = [],
    nego_return,
    parents             = [] :: {atom(), atom()},   % {pidName, status}
    childs              = [],   %dynamicaly added
    inspectors_state    = [],
    loggers_state       = [],
    probe_state        = []
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

% monitor_logger_events
-record(probe_event, {
    id          = 0                 :: integer(),
    insert_ts   = 0                 :: integer(),
    ack_ts      = 0                 :: integer(),
    status      = "undefined"       :: string(),
    textual     = "undefined"       :: string(),
    ack_needed  = true              :: true | false,
    ack_value   = "undefined"       :: string(),
    group_owner = "undefined"       :: string(),
    user_owner  = "undefined"       :: string()
}).

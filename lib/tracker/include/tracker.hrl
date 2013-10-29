-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/file.hrl").
-include("../supercast/include/supercast.hrl").
-include("../errd/include/errd.hrl").

% syslog like security levels
-define(EMERGENCY,  0).
-define(ALERT,      1).
-define(CRITICAL,   2).
-define(ERROR,      3).
-define(WARNING,    4).
-define(NOTICE,     5).
-define(INFO,       6).
-define(DEBUG,      7).

-type hostname()                :: undefined | inet:hostname().
-type ip_add()                  :: undefined | inet:ip_address().
-type probe_id()                :: undefined | integer().
-type probe_type()              :: undefined | fetch | status | set_property.
-type property_key()            :: any().
-type property_val()            :: any().
-type role()                    :: string().
-type seconds()                 :: integer().
-type property()                :: {any(),any()}.
-type tag()                     :: any().
-type target_id()               :: atom().
-type tfun()                    :: fun() | undefined.
-type timeout_alert()           :: fun() | undefined.
-type timeout_threshold()       :: integer() | undefined.
-type oid()                     :: [byte()].

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
    args        = []                    :: [property()]
}).

-record(probe_return, {
    status          = 'UNKNOWN' :: 'OK' | 'UNKNOWN' | 'WARNING' | 'CRITICAL',
    original_reply  = undefined :: string(),
    timestamp       = undefined :: integer(),
    key_vals        = []        :: [{any(), integer()}]
}).

-record(rrd_ds_bind, {
    term    = undefined             :: any(),
    key     = ""                    :: string()
}).

-record(rrd_def, {
    create          = undefined     :: undefined | #rrd_create{},
    update_binds    = undefined     :: undefined | [#rrd_ds_bind{}],
    graph           = ""            :: string()
}).

-record(probe, {
    id                  = undefined     :: probe_id(), % unique in a target
    pid                 = undefined     :: undefined | pid(),
    name                = undefined     :: string(),
    permissions         = #perm_conf{}  :: #perm_conf{},
    tracker_probe_mod   = undefined     :: undefined | module(),
    tracker_probe_conf  = undefined     :: [property()],
    status              = 'UNKNOWN'     :: 'UNKNOWN' | atom(),
    timeout             = 5             :: integer(),
    step                = 60            :: integer(),
    type                = undefined     :: fetch|status|{property, atom()},
    inspectors          = []            :: [#inspector{}],
    loggers             = []            :: [#logger{}],
    properties          = []            :: [property()],

    active              = 1             :: 1 | 0,
    % if it is a snmp probe this field must exist
    snmp_oids           = []            :: [oid()]
}).

-record(target, {
    id          = undefined     :: target_id(),
    global_perm = #perm_conf{
        read        =   ["admin"],
        write       =   ["admin"]
    },
    properties  = [
        {ip,            undefined},
        {hostname,      undefined},
        {sysname,       undefined},
        {snmp_conf,     undefined}
    ]        :: [property()],
    probes      = [] :: [#probe{}],
    directory   = ""
}).

-record(probe_server_state, {
    target,
    probe,
    inspectors_state    = [],
    loggers_state       = []
}).

-record(snmp_conf, {
    ip,
    port        = 161           :: integer(),
    version     = '2c'          :: '2c' | '3',
    seclevel    = none          :: none | auth | enc,
    community   = "none"        :: string(),
    authkey     = none          :: none | string(),
    authalgo    = none          :: none | 'hmac-md5' | 'hmac-sha1',
    enckey      = none          :: none | string(),
    encalgo     = none          :: none | des | aes,
    oid_query   = []            :: [oid()]
}).

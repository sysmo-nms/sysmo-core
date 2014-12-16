-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/file.hrl").
-include("../supercast/include/supercast.hrl").

-define(LOG(X), io:format("{~w, ~w}: DEBUG: ~p~n", [?MODULE, ?LINE, X])).

-define(MASTER_CHANNEL, "target-MasterChan").
-define(PROBES_STATE,   ets_probes_state).
-define(DEFAULT_PERM_CONF, #perm_conf{read=["admin"],write=["admin"]}).
-define(DEFAULT_TARGET_PROPERTIES, [
    {"ip",          "undefined"},
    {"ipVersion",   "undefined"},
    {"sysName",     "undefined"},
    {"dnsName",     "undefined"},
    {"mailAlertgroupL1", ""},
    {"mailAlertgroupL2", ""},
    {"mailAlertEscalationAfter", "1h"},
    {"longitude", 0.0},
    {"latitude",  0.0},
    {"type", "computer"}
]).

-record(dependency, {
    a_probe,
    his_parent
}).

-record(inspector, {
    module,
    conf
}).

-record(logger, {
    module,
    conf
}).

-record(nchecks_probe_conf, {
    function                    :: atom(),
    args        = []            :: [{string(), any()}]
}).

-record(snmp_probe_conf, {
    oids        = []            :: [any()],
    method      = get           :: get | {walk, [string()], [tuple()]},
    retries     = 1             :: integer()
}).

-record(probe_return, {
    status          = "DOWN" :: string(),
    % "OK" | "DOWN" | "WARNING" | "CRITICAL" | "PAUSED",
    % used by inspector status set

    original_reply  = "undefined" :: string(),
    % used by the text logger
 
    timestamp       = 0         :: integer(),
    key_vals        = []        :: [{string(), any()}],
    % used by inspector property set/get 
 
    reply_tuple     = undefined :: any(),
    % used by the rrd logger/walk table
 
    is_event        = false     :: true | false %
    % used by monitor_logger_events app
}).

-record(job, {
    name        = undefined         :: string(),
    belong_to   = "undefined"       :: string(),
    trigger     = "undefined"       :: string(),
    module      = undefined         :: atom(),
    function    = undefined         :: atom(),
    argument    = "undefined"       :: string(),
    info        = "undefined"       :: string(),
    permissions = ?DEFAULT_PERM_CONF :: #perm_conf{}
}).

-record(probe, {
    name                = undefined     :: string(),
    belong_to           = "undefined"   :: string(),
    description         = ""            :: string(),
    info                = ""            :: string(),
    timeout             = 5             :: integer(), % seconds
    status              = "DOWN"     :: string(),
    step                = 300           :: integer(), % seconds
    properties          = []            :: [{string(), any()}],
    active              = true          :: true | false,
    monitor_probe_mod   = undefined     :: undefined | module(),
    monitor_probe_conf  = undefined     :: [any()],
    inspectors          = []            :: [#inspector{}],
    loggers             = []            :: [#logger{}],

    permissions         = ?DEFAULT_PERM_CONF :: #perm_conf{}
}).

-record(target, {
    name        = undefined     :: string(),

    % sys_properties only accessible to users having write access
    sys_properties = []         :: [{atom(), string()}],

    % properties accessible to all users having read access
    properties  = ?DEFAULT_TARGET_PROPERTIES :: [{string(), string()}],

    %probes      = []            :: [#probe{}],
    %jobs        = []            :: [#job{}],

    permissions         = ?DEFAULT_PERM_CONF :: #perm_conf{}
}).

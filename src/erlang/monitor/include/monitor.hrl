-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/file.hrl").
-include("../supercast/include/supercast.hrl").

-define(MASTER_CHANNEL, "monitor_main").
-define(PROBES_STATE,   ets_probes_state).
-define(DEFAULT_PERM_CONF, #perm_conf{read=["admin"],write=["admin"]}).
-define(DEFAULT_TARGET_PROPERTIES, [
    {"host",        "undefined"},
    {"name",        "undefined"},
    {"sysName",     "undefined"},
    {"dnsName",     "undefined"},
    {"mailAlertgroupL1", ""},
    {"mailAlertgroupL2", ""},
    {"mailAlertEscalationAfter", "1h"},
    {"longitude", "0.0"},
    {"latitude",  "0.0"},
    {"type", "computer"}
]).

-define(DEFAULT_SNMP_PROPERTIES, [
    {"snmp_port",     161},
    {"snmp_version",  "2c"},
    {"snmp_seclevel", "noAuthNoPriv"},
    {"snmp_community","public"},
    {"snmp_usm_user", "undefined"},
    {"snmp_authkey",  "undefined"},
    {"snmp_authproto","MD5"},
    {"snmp_privkey",  "undefined"},
    {"snmp_privproto","DES"},
    {"snmp_timeout",  2500},
    {"snmp_retries",  1}
]).

-record(dependency, {
    a_probe,
    his_parent
}).

-record(ets_state, {
    name,
    permissions,
    belong_to,
    tref,
    current_status_from,
    current_status,
    local_state,
    last_return = ""
}).

-record(nchecks_probe_conf, {
    identifier                  :: string(),
    class                       :: string(),
    args        = []            :: [{string(), any()}]
}).

-record(probe_return, {
    status          = "DOWN"        :: string(),
    % "OK" | "DOWN" | "WARNING" | "CRITICAL",
    reply_string    = ""            :: string(),
    reply_code      = 0             :: integer(),
    timestamp       = 0             :: integer(),
    key_vals        = []            :: [{string(), any()}],
    perfs           = []            :: [{string(), integer()}],
    % used by inspector property set/get 
    reply_tuple     = undefined     :: any(),
    % used by the rrd logger/walk table
    opaque          = <<>>          :: any()
}).

-record(job, {
    name        = undefined             :: string(),
    belong_to   = "undefined"           :: string(),
    trigger     = "undefined"           :: string(),
    module      = undefined             :: atom(),
    function    = undefined             :: atom(),
    argument    = "undefined"           :: string(),
    info        = "undefined"           :: string(),
    permissions = ?DEFAULT_PERM_CONF    :: #perm_conf{}
}).

-record(probe, {
    name                = undefined     :: string(),
    belong_to           = "undefined"   :: string(),
    description         = ""            :: string(),
    timeout             = 5             :: integer(), % seconds
    status              = "OK"          :: string(),
    step                = 300           :: integer(), % seconds
    active              = true          :: true | false,
    module              = undefined     :: undefined | module(),
    module_config       = undefined     :: [any()],

    permissions         = ?DEFAULT_PERM_CONF :: #perm_conf{}
}).

-record(target, {
    name        = undefined             :: string(),
    % sys_properties only accessible to users having write access
    sys_properties = []                 :: [{atom(), string()}],
    % properties accessible to all users having read access
    properties  = ?DEFAULT_TARGET_PROPERTIES :: [{string(), string()}],
    permissions = ?DEFAULT_PERM_CONF    :: #perm_conf{}
}).

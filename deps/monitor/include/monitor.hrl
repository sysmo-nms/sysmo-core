-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("supercast/include/supercast.hrl").

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
    {"snmp_port",     "161"},
    {"snmp_version",  "2c"},
    {"snmp_seclevel", "noAuthNoPriv"},
    {"snmp_community","public"},
    {"snmp_usm_user", "undefined"},
    {"snmp_authkey",  "undefined"},
    {"snmp_authproto","MD5"},
    {"snmp_privkey",  "undefined"},
    {"snmp_privproto","DES"},
    {"snmp_timeout",  "2500"},
    {"snmp_retries",  "1"}
]).

-record(dependency, {
    a_probe,
    his_parent
}).

-record(ets_state, {
    name,
    description,
    check_id,
    permissions,
    belong_to,
    tref,
    current_status_from,
    current_status,
    current_status_code,
    local_state,
    last_return = ""
}).

-record(nchecks_probe_conf, {
    identifier                  :: string(),
    class                       :: string(),
    args        = []            :: [{string(), any()}]
}).

% from nchecks/include/nchecks.hrl
% -record(nchecks_reply, {
%     % status = "OK" | "UNKNOWN" | "WARNING" | "CRITICAL" | "ERROR",
%     status          = "UNKNOWN"     :: string(),
%     status_code     = 0             :: integer(),
%     reply_string    = ""            :: string(),
%     timestamp       = 0             :: integer(),
%     performances    = []            :: [{string(), integer()}]
% }).

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
    % status = "OK" | "UNKNOWN" | "WARNING" | "CRITICAL" | "ERROR",
    status              = "UNKNOWN"     :: string(),
    status_code         = 0             :: integer(),
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

-define(CRON_EVERYHOURS, 3600000).
-define(CRON_EVERYDAYS, 86400000).
-define(CRON_EVERY20S,  20000).

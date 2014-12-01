-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/file.hrl").
-include("../supercast/include/supercast.hrl").

-define(LOG(X), io:format("{~w, ~w}: DEBUG: ~p~n", [?MODULE, ?LINE, X])).

-define(MASTER_CHANNEL, "target-MasterChan").

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
    status          = "UNKNOWN" :: string(),
    % "OK" | "UNKNOWN" | "WARNING" | "CRITICAL" | "SHADED" | "PAUSED",
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
    % the name of the job is dynamicaly build using
    % lists:concat([group,module,function,argument])
    % ex: lists:concat(["daily3am", monitor_jobs,hello, target-1234])
    name     = "undefined"      :: string(),
    belong_to = undefined       :: string(),
    trigger  = "undefined"      :: string(),

    module   = undefined        :: atom(),
    function = undefined        :: atom(),
    argument = "undefined"      :: string(),

    info     = "undefined"      :: string(),
    permissions =   #perm_conf{
                        read    = ["admin"],
                        write   = ["admin"]
                    }  :: #perm_conf{}
}).

-record(probe, {
    name                = "undefined"     :: string(),
    belong_to           = "undefined"     :: string(),
    description         = ""            :: string(),
    info                = ""            :: string(),
    permissions         =   #perm_conf{
                                read    = ["admin"],
                                write   = ["admin"]
                            }  :: #perm_conf{},
    timeout             = 5             :: integer(), % seconds
    status              = "UNKNOWN"     :: string(),
    step                = 5             :: integer(), % seconds

    properties          = []            :: [{string(), any()}],
    % forward properties is a list of properties wich will be propagated
    % to the target.
    forward_properties  = []            :: [string()],

    parents             = []            :: [atom()],
    active              = true          :: true | false,

    monitor_probe_mod   = undefined     :: undefined | module(),
    monitor_probe_conf  = undefined     :: [any()],
    inspectors          = []            :: [#inspector{}],
    loggers             = []            :: [#logger{}]
}).

-record(target, {
    name        = "undefined"   :: string(),
    global_perm = #perm_conf{
        read        =   ["admin"],
        write       =   ["admin"]
    },

    % sys_properties only accessible to users having write access
    sys_properties = []         :: [{atom(), string()}],

    % properties accessible to all users having read access
    properties  = []            :: [{any(), any()}],

    probes      = []            :: [#probe{}],
    jobs        = []            :: [#job{}]
}).

% monitor_logger_events
% -record(probe_event, {
%     id          = 0                 :: integer(),
%     insert_ts   = 0                 :: integer(),
%     ack_ts      = 0                 :: integer(),
%     status      = "undefined"       :: string(),
%     textual     = "undefined"       :: string(),
%     ack_needed  = true              :: true | false,
%     ack_value   = "undefined"       :: string(),
%     group_owner = "undefined"       :: string(),
%     user_owner  = "undefined"       :: string()
% }).

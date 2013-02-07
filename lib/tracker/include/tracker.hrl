-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/file.hrl").

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
-type max_timeouts()            :: integer() | undefined.
-type probe_id()                :: undefined | integer().
-type probe_type()              :: undefined | rrd_fetch | status.
-type property()                :: {property_key(), property_val()}.
-type property_key()            :: any().
-type property_val()            :: any().
-type role()                    :: string().
-type rrd_command()             :: string().
-type seconds()                 :: integer().
-type tag()                     :: any().
-type target_id()               :: atom().
-type tfun()                    :: fun() | undefined.
-type timeout_alert()           :: fun() | undefined.
-type timeout_threshold()       :: integer() | undefined.

% can be applied on the targets and the probes
-record(perm_conf, {
    read    = []    :: [role()],
    write   = []    :: [role()]
}).

-record(exceed, {
    type    = undefined         :: undefined | min | max, % < or > value?
    value   = undefined         :: undefined | integer(), % the value,

    % trigger will occur if there is max_chain value exceed in the last
    % max_chain_offset records.
    max_chain = undefined       :: undefined | integer(),
    max_chain_offset = undefined :: undefined | integer()
}).

-record(probe, {
    id              = undefined     :: probe_id(), % unique in each targets
    name            = undefined     :: string(),
    type            = undefined     :: probe_type(), % store rrd data?
    permissions     = #perm_conf{}  :: #perm_conf{},
    func            = undefined     :: tfun(),

    % step, timeouts, timeout before CRITICAL, flip flap detection
    step            = undefined     :: undefined | integer(),
    timeout_wait    = undefined     :: undefined | integer(),
    timeout_max     = undefined     :: undefined | integer(),
    flipflap_mod    = flipflap_off  :: module(),

    % if type = rrd_fetch
    rrd_create          = ""            :: rrd_command(),
    rrd_update          = ""            :: rrd_command(),
    rrd_graph           = ""            :: rrd_command(),
    max_threshold       = undefined     :: undefined | #exceed{},
    min_threshold       = undefined     :: undefined | #exceed{}
}).

-record(target, {
    id          = undefined     :: target_id(),
    ip          = undefined     :: ip_add(),
    hostname    = undefined     :: hostname(),
    global_perm = #perm_conf{
            read = "admin",
            write = "admin"
        }                       :: #perm_conf{},
    probes      = [
        #probe{                     % initial probe_icmp_echo()
            id = 1,
            name = "probe_icmp_echo",
            type = rrd_fetch,
            permissions =   #perm_conf{
                                read = "admin",
                                write = "admin"
                            },
            func = fun(X) -> probe_icmp_echo:exec(X) end,

            % timeouts and frequency
            step            = 5, % 5 seconds
            timeout_wait    = 5, % wait 5 seconds for responce
            timeout_max     = 5, % 5 timeouts trigger an alert


            % Create a rrd for this probe with:
            % - primary data points every 5 seconds (--step 5)
            % DS:
            % - of type GAUGE
            % - with *UNKNOWN* data point if no update for 25 s, do not know 
            % if it is used because a primary point occur after each probe.
            % - with minimum value 0 and max Unknown
            % RRA:
            % - consolide with MAX,
            % - xff is 0,
            % - 1 consolided data point is made from 1 datapoint !!!
            % - can store 3600 points = 3600 * 1 second = 60 minutes
            % - can store 1440 points consolidated with 12 primary = 
            %       12 * 1440 = 17280 seconds = 288 min = 24 h
            rrd_create = "--step 5 DS:latency:GAUGE:25:0:U RRA:MAX:0:1:3600 RRA:MAX:0:12:1440"
        }
    ]                           :: [#probe{}],

    sys_properties  = []        :: [property()],
    sys_tags        = []        :: [tag()],

    properties      = []        :: [property()],
    tags            = []        :: [tag()]
}).

% this record have redundant data in it 2 records of #probe{}. 
% It is here because it is used by the gen_flipflap behaviour modules.
-record(probe_server_state, {
    target_chan     = undefined     :: undefined | pid(),
    target          = undefined     :: undefined | #target{},
    probe           = undefined     :: undefined | #probe{},
    step            = undefined     :: undefined | integer(),
    % max series of timeout ocuring before trigger an alert
    timeout_max     = undefined     :: undefined | integer(),  
    % wait for a responce in timeout_wait
    timeout_wait    = undefined     :: undefined | integer(),
    timeout_current = 0             :: integer(),
    % module implementing the gen_flipflap behaviour will modify this record
    flipflap_state  = undefined     :: any(),
    status          = error         :: error     | ok       % ok | error
}).








-include_lib("kernel/include/inet.hrl").

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
    type            = undefined     :: probe_type(), % store rrd data?
    permissions     = #perm_conf{}  :: #perm_conf{},
    func            = undefined     :: tfun(),

    % frequency and timeouts
    frequency       = undefined     :: undefined | integer(),
    timeout_wait    = undefined     :: undefined | integer(),
    timeout_max     = undefined     :: undefined | integer(),

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
            type = rrd_fetch,
            permissions =   #perm_conf{
                                read = "admin",
                                write = "admin"
                            },
            func = fun(X) -> probe_icmp_echo:exec(X) end,

            % timeouts and frequency
            frequency       = 5, % 5 seconds
            timeout_wait    = 5, % wait 5 seconds for responce
            timeout_max     = 5, % 5 timeouts trigger an alert

            rrd_create = ""
        }
    ]                           :: [#probe{}],

    sys_properties = []         :: [property()],
    sys_tags    = []            :: [tag()],

    properties  = []            :: [property()],
    tags        = []            :: [tag()]
}).



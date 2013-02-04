-include_lib("kernel/include/inet.hrl").

-type rrd_create_com()          :: string().
-type target_id()               :: atom().
-type probe_type()              :: rrd_fetch | status | undefined.
-type timeout_alert()           :: fun() | undefined.
-type timeout_threshold()       :: integer() | undefined.
-type max_timeouts()            :: integer() | undefined.
-type property_key()            :: any().
-type property_val()            :: any().
-type property()                :: {property_key(), property_val()}.
-type tag()                     :: any().
-type seconds()                 :: integer().
-type role()                    :: string().
-type tfun()                    :: fun() | undefined.

-record(perm_conf, {
    read    = []    :: [role()],
    write   = []    :: [role()]
}).

-record(probe, {
    type                = undefined     :: probe_type(), 
    permissions         = #perm_conf{}  :: #perm_conf{},
    rrd_create          = ""            :: rrd_create_com(),
    timeout_threshold   = undefined     :: seconds(),
    max_timeouts        = undefined     :: integer(),
    timeout_alert       = undefined     :: tfun(),
    func                = undefined     :: tfun()
}).

-record(target, {
    id          = undefined     :: target_id(),
    ip          = {0,0,0,0}     :: inet:ip_address(),
    hostname    = "undefined"   :: inet:hostname(),
    global_perm = #perm_conf{}  :: #perm_conf{},
    probes      = []            :: [#probe{}],

    sys_properties = []         :: [property()],
    sys_tags    = []            :: [tag()],

    properties  = []            :: [property()],
    tags        = []            :: [tag()]
}).



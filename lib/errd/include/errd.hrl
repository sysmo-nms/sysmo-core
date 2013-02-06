
-record(rrd_ds_update, {
    name = undefined    :: string(),
    value = unknown     :: any()
}).

-record(rrd_ds, {
    name = undefined        :: string(),
    type = undefined        :: 'gauge' | 'counter' | 'derive' | 'absolute',
    heartbeat = undefined   :: integer(),
    min = undefined         :: integer(),
    max = undefined         :: integer(),
    args = []               :: string()
}).

-record(rrd_rra, {
    cf = undefined      :: 'average' | 'min' | 'max' | 'last',
    args = undefined    :: string()
}).

-record(rrd_create, {
    file = undefined        :: string(),
    start_time = undefined  :: any(), %%%%% TODO TODO TODO
    step = 600              :: pos_integer(),
    ds_defs = []            :: [#rrd_ds{}],
    rra_defs = []           :: [#rrd_rra{}]
}).

-record(rrd_update, {
    file = undefined        :: string(),
    time = now              :: 'now' | string(),
    updates = undefined     :: [#rrd_ds_update{}]
}).

-record(rrd, {
    file = undefined        :: string(),
    ds = []                 :: [#rrd_ds{}],
    rra = []                :: [any()],
    version = undefined     :: string(),
    step = undefined        :: integer(),
    last_update = undefined :: integer()
}).

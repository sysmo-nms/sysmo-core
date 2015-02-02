-define(NICE_WINDOWS,
    [
        {low,           "low"},
        {bellownormal,  "bellownormal"},
        {normal,        "normal"},
        {abovenormal,   "abovenormal"},
        {high,          "high"},
        {realtime,      "realtime"}
    ]
).

-define(NICE_UNIX,
    [
        {low,           "19"},
        {bellownormal,  "10"},
        {normal,        "0"},
        {abovenormal,   "-7"},
        {high,          "-14"},
        {realtime,      "-20"}
    ]
).

-define(HIGH_PRIO_SRV, errd_server_high).
-define(LOW_PRIO_SRV,  errd_server_low).

-define(HIGH_PRIO_CALL_QUEUE, errd_server_call_queue_high).
-define(LOW_PRIO_CALL_QUEUE,  errd_server_call_queue_low).

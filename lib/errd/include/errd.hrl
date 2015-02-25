-define(NICE_WINDOWS,
    [
        {low,           "LOW"},
        {bellownormal,  "BELLOWNORMAL"},
        {normal,        "NORMAL"},
        {abovenormal,   "ABOVENORMAL"},
        {high,          "HIGH"},
        {realtime,      "REALTIME"}
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

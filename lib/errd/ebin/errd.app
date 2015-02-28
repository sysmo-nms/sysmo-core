{application, errd,
    [
        {description, "Erlang bind to librrd"},
        {vsn, "0.1.0"},
        {modules, [
                errd,
                errd_sup,
                errd_app,
                errd_server,
                errd_server_call_queue
            ]
        },
        {registered, 
            [
                errd_server_high,
                errd_server_low,
                errd_sup
            ]
        },
        {applications, 
            [kernel, stdlib]
        },
        {start_phases, []},
        {mod, {errd_app, []}}
    ]
}.

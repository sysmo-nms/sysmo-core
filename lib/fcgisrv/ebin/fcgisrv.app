{application, fcgisrv,
    [
        {description, "Erlang bind to librrd"},
        {vsn, "0.1.0"},
        {modules, [
                fcgisrv_server,
                fcgisrv_sup,
                fcgisrv_app
            ]
        },
        {registered, 
            [
                fcgisrv_server,
                fcgisrv_sup
            ]
        },
        {applications, 
            [kernel, stdlib]
        },
        {start_phases, []},
        {mod, {fcgisrv_app, []}}
    ]
}.

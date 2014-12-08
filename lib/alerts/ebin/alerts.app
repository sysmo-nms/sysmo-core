{application, alerts,
    [
        {description, "Alerts!"},
        {vsn, "0.1.0"},
        {modules, [
                alerts,
                alerts_sup,
                alerts_app
            ]
        },
        {registered, 
            [
                alerts,
                alerts_sup
            ]
        },
        {applications, 
            [kernel, stdlib, monitor]
        },
        {start_phases, []},
        {mod, {alerts_app, []}}
    ]
}.

{application, locator,
    [
        {description, "locate ports on wich a mac/ip address appear"},
        {vsn, "0.1.0"},
        {modules, [
                locator,
                locator_app,
                locator_query,
                locator_query_sup,
                locator_sup,
                locator_event_handler
            ]
        },
        {registered, 
            [
                locator_sup,
                locator_query_sup,
                locator
            ]
        },
        {applications, 
            [kernel, stdlib, monitor, snmp_manager]
        },
        {start_phases, []},
        {mod, {locator_app, []}}
    ]
}.

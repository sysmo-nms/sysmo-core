{application, locator,
    [
        {description, "locate ports on wich a mac/ip address appear"},
        {vsn, "0.1.0"},
        {modules, [
                locator,
                locator_app,
		locator_query,
		locator_query_sup,
                locator_sup
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
            [kernel, stdlib, tracker, snmp_manager]
        },
        {start_phases, []},
        {mod, {locator_app, []}}
    ]
}.

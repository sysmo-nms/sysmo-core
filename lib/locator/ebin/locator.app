{application, locator,
    [
        {description, "locate ports on wich a mac/ip address appear"},
        {vsn, "0.1.0"},
        {modules, [
                locator,
                locator_app,
                locator_sup
            ]
        },
        {registered, 
            [
                locator_sup,
                locator
            ]
        },
        {applications, 
            [kernel, stdlib, nocto_snmpm]
        },
        {start_phases, []},
        {mod, {locator_app, []}}
    ]
}.

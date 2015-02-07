{application, sysmo,
    [
        {description, "Noctopus management system"},
        {vsn, "0.1.0"},
        {modules, [
                sysmo_app,
                sysmo_sup
            ]
        },
        {registered, 
            [
                sysmo_app,
                sysmo_sup
            ]
        },
        {included_applications, 
            [
                mnesia,
                monitor,
                snmpman,
                errd,
                supercast,
                nchecks,
                equartz
            ]
        },
        {applications, 
            [kernel, stdlib]
        },
        {start_phases, []},
        {mod, {application_starter, [sysmo_app, []]}}
    ]
}.

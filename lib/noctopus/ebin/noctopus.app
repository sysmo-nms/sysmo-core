{application, noctopus,
    [
        {description, "Noctopus management system"},
        {vsn, "0.1.0"},
        {modules, [
                noctopus_app,
                noctopus_sup
            ]
        },
        {registered, 
            [
                noctopus_app,
                noctopus_sup
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
                equartz,
                fcgisrv
            ]
        },
        {applications, 
            [kernel, stdlib]
        },
        {start_phases, []},
        {mod, {application_starter, [noctopus_app, []]}}
    ]
}.

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
                monitor,
                snmpman
            ]
        },
        {applications, 
            [kernel, stdlib]
        },
        {start_phases, 
            [
                {first_snmpman, []},
                {second_monitor, []}
            ]
        },
        {mod, {application_starter, [noctopus_app, []]}}
    ]
}.

{application, ipman,
    [
        {description, "Inventory and management of IP address space"},
        {vsn, "0.1.0"},
        {modules, [
                ipman,
                ipman_sup,
                ipman_app
            ]
        },
        {registered, 
            [
                ipman,
                ipman_sup
            ]
        },
        {applications, 
            [kernel, stdlib, tracker, snmp_manager]
        },
        {start_phases, []},
        {mod, {ipman_app, []}}
    ]
}.

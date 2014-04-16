{application, snmp_manager,
    [
        {description, "library implementing the snmpm_user behaviour"},
        {vsn, "0.1.0"},
        {modules, [
                snmp_manager
            ]
        },
        {registered, []},
        {applications, 
            [kernel, stdlib, snmp, monitor]
        }
    ]
}.

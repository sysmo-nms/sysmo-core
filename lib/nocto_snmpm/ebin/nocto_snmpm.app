{application, nocto_snmpm,
    [
        {description, "library implementing the snmpm_user behaviour"},
        {vsn, "0.1.0"},
        {modules, [
                nocto_snmpm_user
            ]
        },
        {registered, []},
        {applications, 
            [kernel, stdlib, snmp, tracker]
        }
    ]
}.

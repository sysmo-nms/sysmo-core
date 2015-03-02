{application, snmpman,
    [
        {description, "Erlang snmp4j manager functions"},
        {vsn, "0.1.0"},
        {modules, [
                snmpman,
                snmpman_guard,
                snmpman_sup,
                snmpman_app
            ]
        },
        {registered, 
            [
                snmpman,
                snmpman_sup
            ]
        },
        {applications, 
            [kernel, stdlib]
        },
        {start_phases, []},
        {mod, {snmpman_app, []}}
    ]
}.

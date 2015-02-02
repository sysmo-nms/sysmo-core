{application, nchecks,
    [
        {description, "Erlang nchecks"},
        {vsn, "0.1.0"},
        {modules, [
                nchecks,
                nchecks_sup,
                nchecks_app
            ]
        },
        {registered, 
            [
                nchecks,
                nchecks_sup
            ]
        },
        {applications, 
            [kernel, stdlib]
        },
        {start_phases, []},
        {mod, {nchecks_app, []}}
    ]
}.

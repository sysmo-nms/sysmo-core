{application, equartz,
    [
        {description, "Erlang equartz"},
        {vsn, "0.1.0"},
        {modules, [
                equartz,
                equartz_sup,
                equartz_app
            ]
        },
        {registered, 
            [
                equartz,
                equartz_sup
            ]
        },
        {applications, 
            [kernel, stdlib]
        },
        {start_phases, []},
        {mod, {equartz_app, []}}
    ]
}.

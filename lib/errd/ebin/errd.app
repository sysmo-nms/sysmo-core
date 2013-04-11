{application, errd,
    [
	    {description, "Erlang Round Robin Databases"},
	    {vsn, "0.1"},
	    {modules, 
	        [
	            errd_app,
	            errdb,
	            errd_command,
	            errd_info,
	            errd_server,
	            errd_server_sup,
	            errd_sup
	        ]
	    },
	    {registered, 
	        [
	            errd_sup, 
	            errd_server_sup
	        ]
	    },
	    {applications, [kernel, stdlib]},
        {start_phases, 
            [
                {initialize_tracker_loggers, []}
            ]
        },
	    {mod, {errd_app, []}}
    ]
}.

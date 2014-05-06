{target,
    'TEMPLATE',
    'TEMPLATE',
    'TEMPLATE',
    [
        {ip,        'TEMPLATE'},
        {location,  'DYNAMIC'},
        {name,      'DYNAMIC'}
    ],
    [
        {probe,
            1,
            undefined,
            'TEMPLATE',
            "ICMP Echo request",
            "Trigger a single echo request every 30 seconds",
            'TEMPLATE',
            bmonitor_probe_nagios,
            {nagios_plugin_conf,
                "/opt/nagios-plugins-1.4.16/libexec/check_icmp",
                [
                    {"-H", 'DYNAMIC'},
                    {"-t", "5"}
                ],
                true
            },
            'UNKNOWN',
            5,
            30,
            [
                {inspector, bmonitor_inspector_status_set,[]},
                {inspector, bmonitor_inspector_property_set, ["status"]}
            ],
            [
                %{logger, bmonitor_logger_events, []},
                {logger, bmonitor_logger_text, []}
            ],
            [],
            [],
            true
        },
        {probe,
            2,
            undefined,
            'TEMPLATE',
            "SNMP sysInfo set",
            "Set the target name and location properties depending on the MIB2
            sysName and sysLocation OIDs every 5 minutes",
            'TEMPLATE',
            bmonitor_probe_snmp,
            {snmp_conf,
                161,
                v2,
                none,
                'TEMPLATE',
                none,
                none,
                none,
                none,
                [
                    {"sysName", [1,3,6,1,2,1,1,5,0]},
                    {"location", [1,3,6,1,2,1,1,6,0]}

                ]
            },
            'UNKNOWN',
            5,
            300,
            [
                {inspector, bmonitor_inspector_status_set,[]},
                {inspector, bmonitor_inspector_property_set, 
                    ["status", "sysName", "location"]}
            ],
            [
                %{logger, bmonitor_logger_events, []},
                {logger, bmonitor_logger_text, []}
            ],
            [],
            [
                {"propertyX", "propertyXValue"},
                {"propertyY", "propertyYValue"}
            ],
            true
        },
        {probe,
            3,
            undefined,
            'TEMPLATE',
            "SNMP interface performances",
            "Query the element MIB-2 interface tree every 5 seconds and store results in a rrd database",
            'TEMPLATE',
            bmonitor_probe_snmp,
            {snmp_conf,
                161,
                v2,
                none,
                'TEMPLATE',
                none,
                none,
                none,
                none,
                [
                    {"sis0in",     [1,3,6,1,2,1,2,2,1,10,1,0]},
                    {"sis0out",    [1,3,6,1,2,1,2,2,1,16,1,0]},
                    {"sis1in",     [1,3,6,1,2,1,2,2,1,10,2,0]},
                    {"sis1out",    [1,3,6,1,2,1,2,2,1,16,2,0]}
                ]
            },
            'UNKNOWN',
            5,
            5,
            [
                {inspector, bmonitor_inspector_status_set,[]},
                {inspector, bmonitor_inspector_property_set, ["status"]}
            ],
            [
                %{logger, bmonitor_logger_events, []},
                {logger, bmonitor_logger_text, []},
                {logger,
                    bmonitor_logger_rrd, [
                        {rrd_config,
                            "secondTestHost-1_rrd1",
                            "create <FILE> --step 5 DS:sis0in:COUNTER:10:U:U DS:sis0out:COUNTER:10:U:U RRA:AVERAGE:0.5:1:600 RRA:AVERAGE:0.5:6:700 RRA:AVERAGE:0.5:24:775 RRA:AVERAGE:0.5:288:797",
                            "update <FILE> --template sis0in:sis0out N:<SIS0-IN>:<SIS0-OUT>",
                            [
                                "DEF:s0in=<FILE>:sis0in:AVERAGE DEF:s0out=<FILE>:sis0out:AVERAGE LINE1:s0in#3465A4 LINE2:s0out#CC0000"
                            ],
                            [
                                {"sis0in",  "<SIS0-IN>"},
                                {"sis0out", "<SIS0-OUT>"}
                            ],
                            none,
                            none
                        },
                        {rrd_config,
                            "secondTestHost-1_rrd2",
                            "create <FILE> --step 5 DS:sis1in:COUNTER:10:U:U DS:sis1out:COUNTER:10:U:U RRA:AVERAGE:0.5:1:600 RRA:AVERAGE:0.5:6:700 RRA:AVERAGE:0.5:24:775 RRA:AVERAGE:0.5:288:797",
                            "update <FILE> --template sis1in:sis1out N:<SIS1-IN>:<SIS1-OUT>",
                            [
                                "DEF:s1in=<FILE>:sis1in:AVERAGE DEF:s1out=<FILE>:sis1out:AVERAGE LINE3:s1in#4E9A06 LINE4:s1out#EDD400"
                            ],
                            [
                                {"sis1in",  "<SIS1-IN>"},
                                {"sis1out", "<SIS1-OUT>"}
                            ],
                            none,
                            none
                        }
                    ]
                }
            ],
            [],
            [
                {"propertyX", "propertyXValue"},
                {"propertyY", "propertyYValue"}
            ],
            true 
        }
    ],
    'TEMPLATE'
}.

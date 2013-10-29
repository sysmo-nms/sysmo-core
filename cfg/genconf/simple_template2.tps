{target,
    'HOSTNAME',
    {perm_conf,     ["admin"], ["admin"]},
    [{ip, ["HOST_IP"]}],
    [
        {probe,
            1,
            undefined,
            'check_icmp-1',
            {perm_conf, ["admin"], ["admin"]},
            btracker_probe_nagios,
            {nagios_plugin_conf,
                "/opt/nagios-plugins-1.4.16/libexec/check_icmp",
                [{"-H", "HOST_IP"}, {"-t", "5"}]},
            'UNKNOWN',
            5,
            4,
            status,
            [
                {inspector, btracker_inspector_standard,[]}
            ],
            [
                {logger, btracker_logger_text, []}
            ],
            [
                {"propertyX", "propertyXValue"},
                {"propertyY", "propertyYValue"}
            ],
            1,
            []
        },
        {probe,
            2,
            undefined,
            'check_http-2',
            {perm_conf, ["admin", "other"], ["admin"]},
            btracker_probe_nagios,
            {nagios_plugin_conf,
                "/opt/nagios-plugins-1.4.16/libexec/check_http",
                [{"-H", "HOST_IP"}, {"-t", "5"}]},
            'UNKNOWN',
            5,
            4,
            status,
            [
                {inspector, btracker_inspector_standard,[]}
            ],
            [
                {logger, btracker_logger_text, []}
            ],
            [
                {"propertyX", "propertyXValue"},
                {"propertyY", "propertyYValue"}
            ],
            1,
            []
        },
        {probe,
            3,
            undefined,
            'snmp-walk-2c',
            {perm_conf, ["admin", "other"], ["admin"]},
            btracker_probe_snmp,
            {snmp_conf,
                "HOST_IP",
                161,
                v2,
                none,
                "public",
                none,
                none,
                none,
                none,
                [
                    [1,3,6,1,2,1,1,5,0]
                ]
            },
            'UNKNOWN',
            5,
            4,
            status,
            [
                {inspector, btracker_inspector_standard,[]}
            ],
            [
                {logger, btracker_logger_text, []}
            ],
            [
                {"propertyX", "propertyXValue"},
                {"propertyY", "propertyYValue"}
            ],
            1,
            []
        }
    ],
    "var/tracker/HOSTNAME"
}.

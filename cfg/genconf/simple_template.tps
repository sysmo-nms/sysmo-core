{target,
    'HOSTNAME',
    {perm_conf,     ["admin", "wheel"], ["admin"]},
    [{ip, ["HOST_IP"]}],
    [
        {probe,
            1,
            undefined,
            'check_icmp-1',
            {perm_conf, ["admin", "wheel"], ["admin"]},
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
        }
    ],
    "var/tracker/HOSTNAME"
}.

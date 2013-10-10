{probe,
    PROBE_ID,
    undefined,
    'PROBE_NAME',
    {perm_conf, ["admin"], ["admin"]},
    btracker_probe_nagios,
    {nagios_plugin,
        "/opt/nagios-plugins-1.4.16/libexec/check_icmp",
        [{"-H", "IP_ADDRESS"}, {"-t", "5"}]},
    'UNKNOWN',
    5,
    4,
    status,
    [
        {inspector, btracker_inspector_standard,[]}
    ],


    [],
    1,
    []
}

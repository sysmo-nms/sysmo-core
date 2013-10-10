{probe,
    PROBE_ID,
    undefined,
    'PROBE_NAME',
    {perm_conf, ["admin"], ["admin"]},
    btracker_probe_nagios,
    {nagios_plugin,
        "/opt/nagios-plugins-1.4.16/libexec/check_http",
        [{"-H", "IP_ADDRESS"}, {"-w", "5"}, {"-c", "100"}]},
    'UNKNOWN',
    5,
    10,
    status,
    [
        {inspector, btracker_inspector_standard,[]}
    ],


    [],
    1,
    []
}

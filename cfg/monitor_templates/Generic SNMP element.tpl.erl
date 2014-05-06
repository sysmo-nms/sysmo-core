{target,
    'TEMPLATE_ID',
    'TEMPLATE_IP',
    'TEMPLATE_PERM',
    [
        {ip,        'TEMPLATE_IP'},
        {location,  'TEMPLATE_LOCATION'},
        {name,      'TEMPLATE_NAME'}
    ],
    [
        {function, monitor_templates, generate_icmpProbe},
        {function, monitor_templates, generate_sysLocNameProbe},
        {function, monitor_templates, generate_ifPerfProbe}
    ],
    'TEMPLATE_DIR'
}.

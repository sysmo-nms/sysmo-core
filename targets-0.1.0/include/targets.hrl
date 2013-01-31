-record(target, {
    id,
    ip,
    hostname,
    probes = [],
    % properties only writable by the system
    sys_properties = [
        {permission_conf, ["admin"], ["admin"]}
    ],    
    sys_tags    = [],       % tags only writable by the system
    properties  = [],       % store user defined properties
    tags        = []        % store user defined tags
}).

-record(permission_conf, {
    read,
    write
}).

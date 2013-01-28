-record(target, {
    id          = undef,
    ip          = undef,
    hostname    = undef,
    sys_properties = [],    % properties only writable by the system
    sys_tags    = [],       % tags only writable by the system
    properties  = [],   % store user defined properties
    tags        = []    % store user defined tags
}).

-record(target, {
    id          = undef,
    ip          = undef,
    hostname    = undef,
    sys_properties = [], % properties only writable by the system
    sys_tags    = [],    % tags only writable by the system
    properties  = [],   % store user defined properties
    lazy_tags   = []    % users defined tags
}).

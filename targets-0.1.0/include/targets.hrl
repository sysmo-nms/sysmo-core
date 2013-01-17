-record(target, {
    id,
    type,
    virtual_groups,
    user_properties,
    lazy_tags,
    target_params,     % list of target_params

-record(target_params, {
    init_mod,        % fun() with target_config as argument
    target_config,   % target_config record
    virtual_groups,
    user_properties,
    lazy_tags
}
-record(target_config, {
    conf_id,        % unique to the target
    ip_address,    
    port,
    protocol,
    proto_conf
}


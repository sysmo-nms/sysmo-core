-record(element_def, {
    name,
    host,
    port,
    snmp_version,
    sec_level,
    retries,
    timeout,
    sec_name,
    community,
    auth_proto,
    auth_key,
    priv_proto,
    priv_key
}).

-define(LOG2(X), io:format("{~p, ~p}: DEBUG: ~p~n", [?MODULE, ?LINE, X])).
-define(SNMPM_USER, "noctopus_snmpm_user").
-define(BULK_MAX_REP, 64).
% MIB2 'system' tree
-define(OID_SYS_DESCR, [1,3,6,1,2,1,1,1,0]).
-define(OID_SYS_OBJECT_ID, [1,3,6,1,2,1,1,2,0]).
-define(OID_SYS_UPTIME, [1,3,6,1,2,1,1,3,0]).
-define(OID_SYS_CONTACT, [1,3,6,1,2,1,1,4,0]).
-define(OID_SYS_NAME, [1,3,6,1,2,1,1,5,0]).
-define(OID_SYS_LOCATION, [1,3,6,1,2,1,1,6,0]).
-define(OID_SYS_SERVICES, [1,3,6,1,2,1,1,7,0]).
-record(services, {
physical,
datalink,
internet,
end_to_end,
application
}).
-record(mib2_system, {
sys_descr,
sys_object_id,
sys_uptime,
sys_contact,
sys_name,
sys_location,
sys_services
}).
% MIB2 'interface' tree
-define(OID_IF_NUMBER, [1,3,6,1,2,1,2,1]).
-define(OID_IF_TABLE, [1,3,6,1,2,1,2,2]).
-define(OID_IF_INDEX, [1,3,6,1,2,1,2,2,1,1]).
-define(OID_IF_DESCR, [1,3,6,1,2,1,2,2,1,2]).
-define(OID_IF_TYPE, [1,3,6,1,2,1,2,2,1,3]).
-define(OID_IF_MTU, [1,3,6,1,2,1,2,2,1,4]).
-define(OID_IF_SPEED, [1,3,6,1,2,1,2,2,1,5]).
-define(OID_IF_PHYS_ADDRESS,[1,3,6,1,2,1,2,2,1,6]).
-define(OID_IF_ADMIN_STATUS,[1,3,6,1,2,1,2,2,1,7]).
-define(OID_IF_OPER_STATUS ,[1,3,6,1,2,1,2,2,1,8]).
-define(OID_IF_LAST_CHANGE ,[1,3,6,1,2,1,2,2,1,9]).
-record(mib2_ifEntry, {
ifIndex,
ifDescr,
ifType,
ifMtu,
ifSpeed,
ifPhysAddress,
ifAdminStatus,
ifOperStatus,
ifLastChange
}).
% MIB2 - 'dot1dBridge' tree (Q-BRIDGE-MIB extention)
-define(OID_DOT1Q_VLAN, [1,3,6,1,2,1,17,7,1,4]).
-define(OID_DOT1Q_AGING_TIME, [1,3,6,1,2,1,17,4,2,0]).
%-define(OID_DOT1Q_FORWARDING_TABLE, [1,3,6,1,2,1,17,4,3]).
% same as above but include the vlan number (?)
% XXX not supported by allied cheaper switchs.
-define(OID_DOT1Q_TPFDB_TABLE, [1,3,6,1,2,1,17,7,1,2,2,1,2]).
-record(dot1q_tpfdb_entry, {
if_index,
vlan,
mac_address
}).
% MIB2 - 'ip' tree (IP-MIB extention)
% ip informations bind to ifIndexes
-define(OID_IP_ADDRESS_TABLE, [1,3,6,1,2,1,4,20]).
-define(OID_IP_INET_TO_MEDIA_TABLE, [1,3,6,1,2,1,4,22]).
-record(inet_to_media_entry, {
inet,
mac,
if_index
}).
% same as above but include last updated time ?
% XXX do it with MediaTable, Alcatel and Allied did not implement
% the PhysicalTable.
-define(OID_IP_INET_TO_PHYSICAL_TABLE, [1,3,6,1,2,1,4,35]).
-record(inet_to_physical_entry, {
inet,
mac,
if_index,
last_updated
}).

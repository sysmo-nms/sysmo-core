define PROBE_TPS
{probe,ID,PID,NAME,PERMISSIONS,TRACKER_PROBE_MOD,TRACKER_PROBE_CONF,\
STATUS,TIMEOUT,STEP,TYPE,INSPECTORS,LOGGERS,PROPERTIES,ACTIVE,SNMP_OIDS}
endef

define TARGET_TPS
{target, ID,GLOBAL_PERM,PROPERTIES,PROBES,DIRECTORY}
endef

# inspectors
INSPECTOR_STANDARD_TPS = "templates/inspectors/template-inspector_standard"

# loggers
LOGGER_TEXT_TPS     = "templates/loggers/template-logger_text"
LOGGER_RRD_TPS      = "templates/loggers/template-logger_rrd"

# probe conf
PROBE_CONF_SNMP     = "templates/probes-conf/template-snmp_conf"
PROBE_CONF_NAGIOS   = "templates/probes-conf/template-nagios_plugin_conf"

compile:
	echo "$(TARGET_TPS)"

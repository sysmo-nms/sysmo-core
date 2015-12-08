-define(IF_INDEX,           "1.3.6.1.2.1.2.2.1.1").
-define(IF_DESCR,           "1.3.6.1.2.1.2.2.1.2").
-define(IF_TYPE,            "1.3.6.1.2.1.2.2.1.3").
-define(IF_MTU,             "1.3.6.1.2.1.2.2.1.4").
-define(IF_SPEED,           "1.3.6.1.2.1.2.2.1.5").
-define(IF_PHYS_ADDRESS,    "1.3.6.1.2.1.2.2.1.6").
-define(IF_ADMIN_STATUS,    "1.3.6.1.2.1.2.2.1.7").
-define(IF_OPER_STATUS,     "1.3.6.1.2.1.2.2.1.8").
-define(IF_LAST_CHANGE,     "1.3.6.1.2.1.2.2.1.9").
-define(IF_IN_OCTETS,       "1.3.6.1.2.1.2.2.1.10").
-define(IF_IN_UCASTPKTS,    "1.3.6.1.2.1.2.2.1.11").
-define(IF_IN_NUCASTPKTS,   "1.3.6.1.2.1.2.2.1.12").
-define(IF_IN_DISCARDS,     "1.3.6.1.2.1.2.2.1.13").
-define(IF_IN_ERRORS,       "1.3.6.1.2.1.2.2.1.14").
-define(IF_IN_UNKNOWN,      "1.3.6.1.2.1.2.2.1.15").
-define(IF_OUT_OCTETS,      "1.3.6.1.2.1.2.2.1.16").
-define(IF_OUT_UCASTPKTS,   "1.3.6.1.2.1.2.2.1.17").
-define(IF_OUT_NUCASTPKTS,  "1.3.6.1.2.1.2.2.1.18").
-define(IF_OUT_DISCARDS,    "1.3.6.1.2.1.2.2.1.19").
-define(IF_OUT_ERRORS,      "1.3.6.1.2.1.2.2.1.20").
-define(IF_OUT_QLEN,        "1.3.6.1.2.1.2.2.1.21").
-define(IF_SPECIFIC,        "1.3.6.1.2.1.2.2.1.22").



-define(MIB2_SYSTEM, "1.3.6.1.2.1.1").
-define(SYS_DESCR,   "1.3.6.1.2.1.1.1.0").
-define(SYS_OBJECTID,"1.3.6.1.2.1.1.2.0").
-define(SYS_UPTIME,  "1.3.6.1.2.1.1.3.0").
-define(SYS_CONTACT, "1.3.6.1.2.1.1.4.0").
-define(SYS_NAME,    "1.3.6.1.2.1.1.5.0").
-define(SYS_LOCATION,"1.3.6.1.2.1.1.6.0").
-define(SYS_SERVICES,"1.3.6.1.2.1.1.7.0").
-define(SYS_ORLAST_CHANGE,"1.3.6.1.2.1.1.8.0").

-define(IF_INFO,
    [?IF_INDEX, ?IF_DESCR, ?IF_TYPE, ?IF_MTU, ?IF_SPEED,
    ?IF_PHYS_ADDRESS, ?IF_ADMIN_STATUS, ?IF_OPER_STATUS,
    ?IF_LAST_CHANGE]).
-define(IF_TABLE, "1.3.6.1.2.1.2.2").
-define(IF_X_TABLE, ".1.3.6.1.2.1.31.1.1").
-define(TMP_AGENT, "testAgent-xxx").

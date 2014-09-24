-define(IF_INDEX, "1.3.6.1.2.1.2.2.1.1").
-define(IF_DESCR, "1.3.6.1.2.1.2.2.1.2").
-define(IF_TYPE,  "1.3.6.1.2.1.2.2.1.3").
-define(IF_MTU,   "1.3.6.1.2.1.2.2.1.4").
-define(IF_SPEED, "1.3.6.1.2.1.2.2.1.5").
-define(IF_PHYS_ADDRESS, "1.3.6.1.2.1.2.2.1.6").
-define(IF_ADMIN_STATUS, "1.3.6.1.2.1.2.2.1.7").
-define(IF_OPER_STATUS,  "1.3.6.1.2.1.2.2.1.8").
-define(IF_LAST_CHANGE,  "1.3.6.1.2.1.2.2.1.9").

-define(MIB2_SYSTEM, "1.3.6.1.2.1.1").
-define(SYS_DESCR,   "1.3.6.1.2.1.1.1").
-define(SYS_OBJECTID,"1.3.6.1.2.1.1.2").
-define(SYS_UPTIME,  "1.3.6.1.2.1.1.3").
-define(SYS_CONTACT, "1.3.6.1.2.1.1.4").
-define(SYS_NAME,    "1.3.6.1.2.1.1.5").
-define(SYS_LOCATION,"1.3.6.1.2.1.1.6").
-define(SYS_SERVICES,"1.3.6.1.2.1.1.7").

-define(IF_INFO,
    [?IF_INDEX, ?IF_DESCR, ?IF_TYPE, ?IF_MTU, ?IF_SPEED,
    ?IF_PHYS_ADDRESS, ?IF_ADMIN_STATUS, ?IF_OPER_STATUS,
    ?IF_LAST_CHANGE]).

-define(TMP_ELEMENT, "testTarget").

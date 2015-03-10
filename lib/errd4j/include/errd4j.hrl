% DATA SOURCE TYPE
-define(DS_GAUGE,   0).
-define(DS_COUNTER, 1).
-define(DS_DERIVE,  2).
-define(DS_ABSOLUTE,3).

% CONSOLIDATION FUNCTION
-define(CF_AVERAGE, 0).
-define(CF_MIN,     1).
-define(CF_MAX,     2).
-define(CF_LAST,    3).
-define(CF_FIRST,   4).
-define(CF_TOTAL,   5).

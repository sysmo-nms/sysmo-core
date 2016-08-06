%%=
%%=
-record(nchecks_reply, {
    status = "UNKNOWN"    :: string(),  % "OK" | "UNKNOWN" | "WARNING" | "CRITICAL" | "ERROR",
    status_code = 0     :: integer(),
    performances = []    :: [{Id::string(), [{Db::string, Value::integer()}]}],
    reply_string = ""   :: string(),
    timestamp = 0       :: integer()
}).

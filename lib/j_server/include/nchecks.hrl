%

-record(nchecks_reply, {
    status :: string(),  % "OK" | "UNKNOWN" | "WARNING" | "CRITICAL" | "ERROR",
    status_code :: integer(),
    performances :: [{Id::string(), [{Db::string, Value::integer()}]}],
    reply_string :: string(),
    timestamp :: integer()
}).

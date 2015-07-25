%
-define(LOG_INFO(String,Term),
    error_logger:info_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String},
       {term, Term}])).

-define(LOG_INFO(String),
    error_logger:info_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String}])).

-define(LOG_WARNING(String,Term),
    error_logger:warning_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String},
       {term, Term}])).


-define(LOG_WARNING(String),
    error_logger:warning_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String}])).

-define(LOG_ERROR(String,Term),
    error_logger:error_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String},
       {term, Term}])).

-define(LOG_ERROR(String),
    error_logger:error_report([
       {module, ?MODULE},
       {line, ?LINE},
       {message, String}])).

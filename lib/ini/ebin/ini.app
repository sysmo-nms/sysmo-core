{application, ini, [
    {description, "An Erlang INI parser"},
    {vsn, "0.1"},
    {modules, [
        ini,
        ini_parser,
        ini_lexer
    ]},
    {applications, [kernel, stdlib]},
    {registered, []}
]}.

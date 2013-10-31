{application,errd,
             [{description,"Erlang Round Robin Databases"},
              {vsn,"0.2"},
              {applications,[kernel,stdlib]},
              {mod,{errd_app,[]}},
              {registered,[errd_sup, errd_server_sup, errd_server]},
              {modules,[
                errd_app,
                errd_command,
                errd_info,
                errd_server,
                errd_sup,
                errdb]}]}.

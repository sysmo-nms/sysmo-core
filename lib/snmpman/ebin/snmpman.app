{application,snmpman,
             [{description,"Erlang snmp4j manager functions"},
              {vsn,"0.1.0"},
              {modules,[snmpman,snmpman_app,snmpman_guard,snmpman_sup]},
              {registered,[snmpman,snmpman_sup]},
              {applications,[kernel,stdlib]},
              {start_phases,[]},
              {mod,{snmpman_app,[]}}]}.

Architecture
============

Sysmo is composed of several Erlang Applications running in a single Erlang VM. They are supervised by a master application (sysmo_app). A Java VM is open on startup (j_server) to offer various services.

In addition, Sysmo-Workers can connect to the jserver application to offer ressources for executing NChecks actions.

# Diagram
```text

+------------------------------------------------------+
| Erlang VM     +-------+                              |
|        +------+ sysmo +---------------------+        |
|        |      +---+---+                     |        |
|        |          |                         |        |  +---------------+
|   +----+---+      |    +--------+      +----+----+   |  | Potentially X |
|   | monitor|      +----+ mnesia |      | jserver +------| Sysmo-Workers |
|   +----+---+           +--------+      +----+----+   |  +---------------+
|        |                                    |        |
|   +---+-------+                             |        |
|   | supercast +-------+                     |        |
|   +----+------+       |                     |        |
|        |              |                     |        |
|   +----+---+      +---+---+                 |        |
|   | cowboy |      | ranch |                 |        |
|   +--------+      +-------+                 |        |
+---------------------------------------------|--------+
                                              |
     Network link (erlang node over loopback) |
                                              |
+---------------------------------------------|--------+
| Java VM                               +-----+----+   |
|                                       | j_server |   |
|                                       +-----+----+   |
|                                             |        |
|       +------------+-------------+----------+--+     |
|       |            |             |             |     |
|   +---+---+   +---+---+   +----+---+   +----+----+   |
|   | rrd4j |   | derby |   | snmp4j |   | nchecks |   |
|   +-------+   +-------+   +--------+   +---------+   |
+------------------------------------------------------+

```

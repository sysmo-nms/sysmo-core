Sysmo Core
==========
[![Build Status](https://travis-ci.org/sysmo-nms/sysmo-core.svg?branch=master)](https://travis-ci.org/sysmo-nms/sysmo-core)
[![Appveyor Build Status](https://ci.appveyor.com/api/projects/status/github/sysmo-nms/sysmo-core?branch=master&svg=true)](https://ci.appveyor.com/project/ssbx/sysmo-core)

This is the main repository of sysmo-core, server part of the [Sysmo NMS](http://www.sysmo.io) monitoring and management solution.

More information and documentation can be found at the Sysmo [Wiki](https://github.com/sysmo-nms/sysmo-nms.github.io/wiki).

Build
=====
To interact with the server, you will need to download/install our fantastic [Sysmo-operator UI](http://www.sysmo.io/Downloads) user interface.

### Dependencies
To build from sources, you will need the following dependencies:
* Git
* Java JDK version 7 minimum
* A complete Erlang installation
* A Go compiler >= 1.4
* Ruby Rake

Sysmo is actually developed with Openjdk-7, Erlang-17.6 and Go-1.4.2. Other setup should work but are not tested.

### Building

When the required dependencies are installed run the following commands:
```sh
$ git clone https://github.com/sysmo-nms/sysmo-core.git
$ cd sysmo-core
$ git submodule update --init
$ rake release
...
```
A new fresh release is created in the ._build/default/rel/sysmo directory. You can use the executable to test it from here:
```sh
$ ./_build/default/rel/sysmo/bin/sysmo usage
Usage: sysmo {start|start_boot <file>|foreground|stop|restart|reboot|ping|console|getpid|console_clean|console_boot <file>|attach|eval|remote_console|upgrade}
$ ./rel/sysmo/bin/sysmo console
```
Type "q()." to quit the console.

### Deploying

##### Runtime dependencies
Once built, the only runtime dependency is a Java-JRE >= 7.


##### Install #####
There is no "install" target, so extract the generated package archive to your install location ("/opt" for example).
```sh
$ rake pack
$ tar -C /opt -xzf _build/sysmo-x64.tgz
```
The following command will update the "admin" password, and generate a random Erlang Cookie (See the [Erlang documentation concerning cookies](http://erlang.org/doc/reference_manual/distributed.html#id87463) if you want to know more about it).
```sh
$ /opt/sysmo/bin/sysmo_relutils --update_cookie --update_admin_password="secretpassword"
```

##### Permissions #####
The portable ping executable must have root privileges if Sysmo-core is executed from a non root user.

```sh
$ sudo chown root /opt/sysmo/utils/pping
$ sudo chmod 4755 /opt/sysmo/utils/pping
```

Of course the user running the daemon must have read-write permission on /opt/sysmo.

#### Start ####
```sh
$ /opt/sysmo/bin/sysmo start
```

### What's next
If you plan to have a huge number of elements to monitor, you can see the documentation for installing aditionnal workers at [[Deploying Sysmo Workers]].


Development Mode
================
The common workflow when working on Sysmo-Core, is:

Modify erlang or java code, I actually use IntelliJ for both languages then,

Build a local debug release ("rake rel" is a shortcut to this)
```sh
$ rake debug_release
```

Start the server,
```sh
$ rake run
[...]
```

Or in one pass:
```sh
$ rake rel run
```

Note that:
- The release is located at _build/debug/rel/sysmo/,
- The logs are located at _build/debug/rel/sysmo/log/,
- pping is not setuid so CheckICMP will raize a "permission denied error",

Have fun!


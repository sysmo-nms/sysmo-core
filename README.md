# Sysmo Community BETA
Sysmo is a versatile solution unifying monitoring, management and system
scripting. It fallow the development process of Sysmo Enterprise Edition.
For a complete description see [sysmo.io](http://sysmo.io).

# Download prebuild binary
If you want to see how Sysmo work, we suggest that you download the prebuilt
Enterprise Edition binary (for Windows and Linux) available [here](http://sysmo.io).

# Build/Testing Sysmo

In the following procedure you will:
* Build Sysmo Community
* Build the Python API module
* Add some elements to monitore using the Python API
* Create a basic web view using the Python API


## Dependencies
There is actualy not any configure script to build Sysmo. The dependencies
listed here are taken from the root Makefile. If your system use another path
(wich is highly probable), edit the Makefile to fit your system.

### Tools
The build system expect the following tools to be present:
* erl, erlc (under /opt/erlang_otp_17.4/bin/)
* java, javac (under /opt/jdk1.8.0_31/bin/)
* go (version 1.4.1 in your $PATH)
* rrdtool (tested with 1.4.7 in your $PATH)

### Java jars
Dependencies:
* OtpErlang.jar (must be the one present in your erlang installation)
* snmp4j-2.3.1
* quartz-2.2.1 (release archive contain log4j, c3p0, and slf4j)

From the root repository running find should return exactly this:
```sh
$find . -name "*jar"
./lib/nchecks/java_lib/OtpErlang.jar
./lib/snmpman/java_lib/snmp4j-2.3.1.jar
./lib/snmpman/java_lib/OtpErlang.jar
./lib/equartz/java_lib/quartz-jobs-2.2.1.jar
./lib/equartz/java_lib/log4j-1.2.16.jar
./lib/equartz/java_lib/c3p0-0.9.1.1.jar
./lib/equartz/java_lib/slf4j-api-1.6.6.jar
./lib/equartz/java_lib/OtpErlang.jar
./lib/equartz/java_lib/slf4j-log4j12-1.6.6.jar
./lib/equartz/java_lib/quartz-2.2.1.jar
```

### Python
You will require the development files for python2.7 to build the API module.
Depending your OS this package may have different name.
Try (rpm based system):
```sh
$ yum search python2.7 dev
$ sudo yum install XYZ
```
or (deb based system):
```sh
$ apt-cache search python2.7 dev
$ sudo apt-get install XYZ
```

## Build

First get Sysmo source tree:
```sh
$ git clone https://github.com/ssbx/noctopus.git
$ cd noctopus
```

Then run make:
```sh
$ sudo make
```
Make require root privilege because the pping (portable ping) executable must be
setuid. You may run it as normal user and set it yourself if you prefer.

And everything should be fine. Note that the build system will clone the yaws
repository and build it. If you have build errors here, you might need to
fullfil some yaws dependencies.

## Start
```sh
$ make start
```
will build and start an Erlang "local-relase". You shoud see a prompt like this:

```sh
Erlang/OTP 17 [erts-6.3] [source] [64-bit] [smp:1:1] [async-threads:10] [hipe] [kernel-poll:false]

receive init
receive init
receive init

=INFO REPORT==== 5-Feb-2015::10:17:48 ===
Yaws: Listening to 0.0.0.0:8080 for <1> virtual servers:
 - http://supercast:8080 under var/yaws/docroot
Eshell V6.3  (abort with ^G)
(master@debian)1> 
```

## Adding elements
Open a new terminal:
```sh
$ cd $(your sysmo root repository)/lib/pysysmo
$ vi myscript.py
```


Here are some examples of the pysysmo API module:

	#!/usr/bin/env python
	
	from __init__ import *
	
	# see http://sysmo.io/documentation/pysysmoTutorial.html
	# see http://sysmo.io/documentation/pysysmo.html
	# see http://sysmo.io/documentation/pysysmo/class/Target.html
	# see http://sysmo.io/documentation/pysysmo/class/Probe.html

	sysmo = Server()
	sysmo.connect()
	
	# add a web server
	ghub = Target("www.github.com")
	ghub.addProbe(Probe("icmp", conf=default))
	ghub.addProbe(Probe("http", conf=default))
	ghub.setProperty('mailAlertGroup', 'mymail@mymail.net')
	ghub.setProperty('mailAlertGroup2', 'mymail2@mymail.net')
	ghub.setProperty('mailAlertEscalation', '60')
	ghub.setProperty('my property', 'web server')

	# include our web server target some tie script
	# see http://sysmo.io/documentation/TieScriptTutorial.html
	ghub.addTieScript("mywikipages.py")
	ghub.addTieScript("myinventorytool.py")
	ghub.addTieScript("anyexternaltool.py")

	# update the server with this target
	sysmo.update(ghub)
	

	# add a network element
	netelement = Target("192.168.0.254")
	netelement.addProbe(Probe("icmp", conf=default))
	netelement.setSnmpVersion('2c')
	netelement.setSnmpCommunity('public')
	sysmo.update(netelement)
	# begin to log all interfaces to rrds
	ifList = sysmo.getIfInfosFor(netelement)
	netelement.setIfLogList(ifList)
	sysmo.update(netelement)

	# modify the default values for my web server
	# see http://sysmo.io/documentation/graphPerformances.html
	# step 1: get all elements aving property key 'my property'
	elems = sysmo.getElementByPropertyKey('my property')
	# step 2: filter to have only 'web server' value
	webserv = list()
	for e in elems:
		val = e.getValueForProperty('my property')
		if val == 'web server': webserv.append(e)
	
	for w in webserv:
		probes = f.getProbesByType("http")
		for p in probes:
			p.setTimeout(10000)
		sysmo.update(w)
	
	
	sysmo.close()
	
Then run:
```sh
$ python myscript.py
```


## Create a very succint web interface
This script must be put in the cgi yaws directory $(your root repository)/var/yaws/cgi/ and accessed http://localhost:8080/cgis/myscript.py
	
```sh
$ vi mycgi.py
```
	from __init__ import *
	
	sysmo = Server()
	sysmo.connect()
	elements = sysmo.getAll()
	sysmo.close()
	
	# see http://sysmo.io/documentation/pysysmo/class/Target.html
	# see http://sysmo.io/documentation/pysysmo/class/Probe.html

	print "<doctype html>"
	print "<html><body>"
	print "<h4>Powered by Sysmo</h4>"
	for e in elements:
	    print "<h2>", e.printableName(), "</h2>"
	    print "<table>"
	
	    for p in e.getProbes():
	        print "<td>"
	        print "<tr>", p.getPrintableName(), "</tr>"
	        print "<tr>", p.getCurrentStatus(), "</tr>"
	        print "<tr>", p.getNextCheck(unit='seconds'), "</tr>"
	        print "</td>"
	   print "</table>"
	    
	print "</body></html>"

You can see the result at http://localhost:8080/cgi/mycgi.py.

Here you are. You can now build your how web interface and include some form to add
targets to the system with the preceding script example. Enjoy!


# License
Sysmo Community is release under the Common Public Attribution License. See LICENSE.TXT.

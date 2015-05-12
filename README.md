Sysmo Service
=============

# Building
## Build dependencies
* java JDK 8 or later
* A complete erlang installation 16 or later
* A go compiler 1.4.1 or later
Sysmo is developped with oracle-jdk-8, erlang-17.5 and go-1.4.2. Other setup
should work but are not tested.

## Build procedure
### Download
```sh
git clone https://github.com/ssbx/sysmo-CE.git
cd sysmo-CE
export SYSMO_BUILD_DIRECTORY=`pwd`
```

### Make
```sh
cd $SYSMO_BUILD_DIRECTORY
make rel
```
The "$SYSMO_BUILD_DIRECTORY/sysmo" directory now contain a complete Sysmo Release.

# Install
There are no automation tools for installing Sysmo at the moment.
* The portable ping implementation must have high privileges and be setuid:
```sh
cd $SYSMO_BUILD_DIRECTORY
chown root:root ./sysmo/utils/pping
chmod +s ./sysmo/utils/pping
```
* Move the ./sysmo directory to your install location (ie: /opt/sysmo or /srv/sysmo),
```sh
cd $SYSMO_BUILD_DIRECTORY
sudo mv ./sysmo /srv/
```
* Edit one of the System V init scripts available in $SYSMO_BUILD_DIRECTORY/script/init/. You should
set the absolute path of the "sysmo" executable (ie: "/opt/sysmo/bin/sysmo" or
"/srv/sysmo/bin/sysmo") to $SYSMO_SCRIPT variable.
* Copy $SYSMO_BUILD_DIRECTORY/scripts/sysmo-yaws.conf.tpl to the yaws conf.d/sysmo-yaws.conf and
replace @sysmoDocRoot@ string by the release location docroot
(ie: /opt/sysmo/docroot or /src/sysmo/docroot). (If you use httpd, follow the
same procedure with sysmo-httpd.conf.tpl).

# Running
## Runtime dependencies
* java RE 8 or later
* yaws or apache2 webserver
If installed from source you will need to reload your webserver configuration
and install/ start the sysmo service using your service manager
(see $SYSMO_BUILD_DIRECTORY/scripts/init/<YourOSFamily>/README).

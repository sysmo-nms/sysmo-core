#!/bin/sh

set -e

rm -rf rel/sysmo/cfg/nchecks
rm -rf rel/sysmo/var/docroot/nchecks

cp -R rel/sysmo/java_apps/nchecks/defs rel/sysmo/cfg/nchecks
cp -R rel/sysmo/java_apps/nchecks/defs rel/sysmo/var/docroot/nchecks

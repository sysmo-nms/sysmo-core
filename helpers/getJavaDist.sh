#!/bin/sh

rm -rf rel/sysmo/java_dist
mkdir rel/sysmo/java_dist
for app in errd4j snmpman nchecks equartz; do
    cp -R java/${app}/build/install/${app} rel/sysmo/java_dist/
done

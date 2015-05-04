#!/bin/sh

rm -rf rel/sysmo/utils
mkdir rel/sysmo/utils

if [ -e pping/pping.exe ]
then
    cp go/pping.exe rel/sysmo/utils/
else
    cp go/pping rel/sysmo/utils/
fi

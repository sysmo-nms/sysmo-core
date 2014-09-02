#!/bin/sh

sudo erl -sname manager -config snmp/manager -eval "application:start(crypto), application:start(snmp)"

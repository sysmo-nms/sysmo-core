#!/bin/sh

sudo /home/seb/src/otp/bin/erl -sname manager -config snmp/manager -eval "application:start(crypto), application:start(snmp)"

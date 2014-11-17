%% Generated by the Erlang ASN.1 compiler version:3.0.1
%% Purpose: Erlang record definitions for each named and unnamed
%% SEQUENCE and SET, and macro definitions for each value
%% definition,in module Monitor



-ifndef(_MONITOR_HRL_).
-define(_MONITOR_HRL_, true).

-record('IpInfo',{
version, stringVal}).

-record('PermConf',{
read, write}).

-record('Property',{
key, value}).

-record('Inspector',{
module, conf}).

-record('LoggerRrd2',{
module, type, rrdCreate, rrdUpdate, rrdGraphs, indexes}).

-record('LoggerText',{
module, conf}).

-record('LoggerEvents',{
module, conf}).

-record('ProbeConf',{
name, type}).

-record('ProbeInfo',{
channel, name, descr, info, permissions, probeMod, probeConf, status, timeout, step, inspectors, loggers, properties, active, infoType}).

-record('ProbeEvent',{
probeName, eventId, insertTs, ackTs, status, textual, ackNeeded, ackValue, groupOwner, userOwner}).

-record('ProbeReturn',{
target, probeId, status, originalReply, timestamp, keysVals, nextReturn}).

-record('TargetInfo',{
channel, properties, type}).

-record('CreateTarget',{
ipAdd, permConf, staticName, snmpv2ro, snmpv2rw, template, queryId}).

-record('CreateSimpleProbe',{
target, name, description, permConf, template, timeout, step, flags, exe, queryId}).

-record('Query',{
queryId, query}).

-record('Arg',{
flag, value}).

-record('SimulateCheck',{
queryId, executable, args}).

-record('UpdateTarget',{
target}).

-record('DeleteTarget',{
target}).

-record('MonitorReply',{
queryId, status, info}).

-record('GetCheckReply',{
queryId, status, infos}).

-endif. %% _MONITOR_HRL_
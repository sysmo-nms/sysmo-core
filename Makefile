export MODS = supercast monitor errd4j snmpman nchecks equartz pping 

compile:
	rebar -r compile
	$(MAKE) -C go
	$(MAKE) -C java
	
test:
	rebar -r test
	$(MAKE) -C go test
	$(MAKE) -C java test

doc:
	rebar -r doc
	$(MAKE) -C go doc
	$(MAKE) -C java doc

clean:
	rebar -r clean
	$(MAKE) -C go clean
	$(MAKE) -C java clean

rel: compile
	cd rel; rebar generate

EPATH = ebin \
lib/equartz/ebin \
lib/errd4j/ebin \
lib/monitor/ebin \
lib/nchecks/ebin \
lib/snmpman/ebin \
lib/supercast/ebin 

CONFIG = ./sys

start:
	erl -sname master -config ./dev -pa $(EPATH)

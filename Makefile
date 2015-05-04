.PHONY: rel

export MAKE  ?= make
export REBAR ?= rebar

compile:
	$(REBAR) -r compile
	$(MAKE) -C go
	$(MAKE) -C java
	
test:
	$(REBAR) -r test
	$(MAKE) -C go test
	$(MAKE) -C java test

doc:
	$(REBAR) -r doc
	$(MAKE) -C go doc
	$(MAKE) -C java doc

clean:
	$(REBAR) -r clean
	$(MAKE) -C go clean
	$(MAKE) -C java clean

rel: 
	$(REBAR) -r compile
	$(MAKE) -C go
	$(MAKE) -C java dist
	cd rel; $(REBAR) generate
	./scripts/placeJavaDist.sh
	./scripts/placeGoUtils.sh
	./scripts/placeNchecksXml.sh

rel-start:
	./rel/sysmo/bin/sysmo start
rel-attach:
	./rel/sysmo/bin/sysmo attach
rel-stop:
	./rel/sysmo/bin/sysmo stop

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

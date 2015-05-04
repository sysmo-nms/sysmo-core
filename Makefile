.PHONY: rel

export MAKE  ?= make
export REBAR ?= rebar

compile:
	cd src/erlang/sysmo; $(REBAR) -r compile
	$(MAKE) -C src/go
	$(MAKE) -C src/java
	
test:
	cd src/erlang/sysmo; $(REBAR) -r test
	$(REBAR) -r test
	$(MAKE) -C src/go test
	$(MAKE) -C src/java test

check:
	$(MAKE) -C src/java check

doc:
	cd src/erlang/sysmo; $(REBAR) -r doc
	$(REBAR) -r doc
	$(MAKE) -C src/go doc
	$(MAKE) -C src/java doc

clean:
	$(REBAR) clean
	cd src/erlang/sysmo; $(REBAR) -r clean
	$(MAKE) -C src/go clean
	$(MAKE) -C src/java clean

rel: 
	cd src/erlang/sysmo; $(REBAR) -r compile
	$(MAKE) -C src/go
	$(MAKE) -C src/java dist
	$(REBAR) generate
	./scripts/placeReleaseFiles
	@ echo "Release ready"

rel-start:
	./sysmo/bin/sysmo start
rel-attach:
	./sysmo/bin/sysmo attach
rel-stop:
	./sysmo/bin/sysmo stop

EPATH = ebin \
src/erlang/equartz/ebin \
src/erlang/errd4j/ebin \
src/erlang/monitor/ebin \
src/erlang/nchecks/ebin \
src/erlang/snmpman/ebin \
src/erlang/supercast/ebin 

CONFIG = ./sys

start:
	erl -sname master -config ./dev -pa $(EPATH)

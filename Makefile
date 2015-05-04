.PHONY: rel

export MAKE  ?= make
export REBAR ?= rebar
export GRADLE ?= gradle

ERLANG_SRC = src/erlang/sysmo
JAVA_SRC = src/java/sysmo_java
GO_SRC = src/go

compile:
	cd $(ERLANG_SRC); $(REBAR) -r compile
	cd $(JAVA_SRC); $(GRADLE) classes
	$(MAKE) -C src/go
	
test:
	cd $(ERLANG_SRC); $(REBAR) -r test
	cd $(JAVA_SRC); $(GRADLE) test
	$(REBAR) test
	$(MAKE) -C src/go test

check:
	cd $(JAVA_SRC); $(GRADLE) check

doc:
	cd $(ERLANG_SRC); $(REBAR) -r doc
	cd $(JAVA_SRC); $(GRADLE) doc
	$(REBAR) doc
	$(MAKE) -C src/go doc

clean:
	cd $(ERLANG_SRC); $(REBAR) -r clean
	cd $(JAVA_SRC); $(GRADLE) clean
	$(REBAR) clean
	$(MAKE) -C src/go clean

rel: 
	cd $(ERLANG_SRC); $(REBAR) -r compile
	cd $(JAVA_SRC); $(GRADLE) installDist
	$(MAKE) -C src/go
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

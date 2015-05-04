.PHONY: rel

export MAKE  ?= make
export REBAR ?= rebar
export GRADLE ?= ./gradlew

ERLANG_SRC = src/erlang/sysmo
JAVA_SRC = src/java/sysmo_java
GO_SRC = src/go

compile:
	cd $(ERLANG_SRC); $(REBAR) -r compile
	cd $(JAVA_SRC); $(GRADLE) classes
	
test:
	cd $(ERLANG_SRC); $(REBAR) -r test
	cd $(JAVA_SRC); $(GRADLE) test
	$(REBAR) test

check:
	cd $(JAVA_SRC); $(GRADLE) check

doc:
	cd $(ERLANG_SRC); $(REBAR) -r doc
	cd $(JAVA_SRC); $(GRADLE) doc
	$(REBAR) doc

clean:
	$(MAKE) -C src/go clean
	cd $(ERLANG_SRC); $(REBAR) -r clean
	cd $(JAVA_SRC); $(GRADLE) clean
	$(REBAR) clean

rel: 
	$(MAKE) -C src/go
	cd $(ERLANG_SRC); $(REBAR) -r compile
	cd $(JAVA_SRC); $(GRADLE) installDist
	$(REBAR) generate
	./scripts/placeReleaseFiles
	@ echo "Release ready"

rel-start:
	./sysmo/bin/sysmo start
rel-attach:
	./sysmo/bin/sysmo attach
rel-stop:
	./sysmo/bin/sysmo stop
run: rel
	./sysmo/bin/sysmo console
	


EPATH = ebin \
src/erlang/equartz/ebin \
src/erlang/errd4j/ebin \
src/erlang/monitor/ebin \
src/erlang/nchecks/ebin \
src/erlang/snmpman/ebin \
src/erlang/supercast/ebin 

CONFIG = ./sys


.PHONY: rel

MAKE  ?= make
REBAR ?= rebar
GRADLE ?= ./gradlew

ERLANG_SRC = src/erlang
JAVA_SRC = src/java
GO_SRC = src/go

# rebar realpath
REBARR = $(realpath $(ERLANG_SRC)/$(REBAR))

compile:
	cd $(ERLANG_SRC)/sysmo; $(REBARR) -r compile
	cd $(JAVA_SRC); $(GRADLE) classes
	
test:
	cd $(ERLANG_SRC)/sysmo; $(REBARR) -r test
	cd $(JAVA_SRC); $(GRADLE) test
	$(REBARR) test

check:
	cd $(JAVA_SRC); $(GRADLE) check

doc:
	cd $(ERLANG_SRC)/sysmo; $(REBARR) -r doc
	cd $(JAVA_SRC); $(GRADLE) doc
	$(REBARR) doc

clean:
	$(MAKE) -C src/go clean
	cd $(ERLANG_SRC)/sysmo; $(REBARR) -r clean
	cd $(JAVA_SRC); $(GRADLE) clean
	$(REBARR) clean

rel: 
	$(MAKE) -C src/go
	cd $(ERLANG_SRC)/sysmo; $(REBARR) -r compile
	cd $(JAVA_SRC); $(GRADLE) installDist
	$(REBARR) generate
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


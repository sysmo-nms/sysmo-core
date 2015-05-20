# sysmo build

REBAR     ?= rebar
REBAR_CMD  = $(realpath src/erlang/$(REBAR))

GRADLE    ?= gradlew
GRADLE_CMD = $(realpath src/java/$(GRADLE))

GO        ?= go
PPING_OUT ?= pping

build:
	cd src/go; $(GO) build -o $(PPING_OUT) pping.go
	cd src/erlang/sysmo; $(REBAR_CMD) -r compile
	cd src/java; $(GRADLE_CMD) installDist
	
test:
	cd src/erlang/sysmo; $(REBAR_CMD) -r test
	cd src/java; $(GRADLE_CMD) test
	$(REBAR_CMD) test

check:
	cd src/java; $(GRADLE_CMD) check

doc:
	cd src/erlang/sysmo; $(REBAR_CMD) -r doc
	cd src/java; $(GRADLE_CMD) doc
	$(REBAR_CMD) doc

clean:
	rm -f src/go/$(PPING_OUT)
	cd src/erlang/sysmo; $(REBAR_CMD) -r clean
	cd src/java; $(GRADLE_CMD) clean
	$(REBAR_CMD) clean

rel: build
	$(REBAR_CMD) generate
	./scripts/placeReleaseFiles
	@ echo "Release ready."

# UTILS
run: rel
	./sysmo/bin/sysmo console
#rel-start:
	#./sysmo/bin/sysmo start
#rel-attach:
	#./sysmo/bin/sysmo attach
#rel-stop:
	#./sysmo/bin/sysmo stop
#setuid_pping:
#	chown root:root sysmo/utils/$(PPING_OUT)
#	chmod +s sysmo/utils/$(PPING_OUT)

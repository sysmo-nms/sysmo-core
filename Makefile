# sysmo build

REBAR_EXE  ?= rebar
REBAR       = $(realpath src/erlang/$(REBAR_EXE))

GRADLE_EXE ?= gradlew
GRADLE      = $(realpath src/java/$(GRADLE_EXE))

GO         ?= go
PPING_OUT  ?= pping

build:
	cd src/go; $(GO) build -o $(PPING_OUT) pping.go
	cd src/erlang/sysmo; $(REBAR) -r compile
	cd src/java; $(GRADLE) installDist

test:
	cd src/erlang/sysmo; $(REBAR) -r test
	cd src/java; $(GRADLE) test

check:
	cd src/java; $(GRADLE) check

doc:
	cd src/erlang/sysmo; $(REBAR) -r doc
	cd src/java; $(GRADLE) doc

clean:
	rm -f src/go/$(PPING_OUT)
	cd src/erlang/sysmo; $(REBAR) -r clean
	cd src/java; $(GRADLE) clean
	$(REBAR) clean

rel: build
	$(REBAR) generate
	./scripts/placeReleaseFiles
	@ echo "Release ready."

run: rel
	./sysmo/bin/sysmo console

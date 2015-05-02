# Makefile

GRADLE ?= gradle --daemon
REBAR ?= rebar

.PHONY: compile rebar-compile clean doc start assemble check dist


compile: rebar-compile java-assemble

rebar-compile:
	$(REBAR) compile

java-assemble:
	$(GRADLE) assemble
	cp build/libs/nchecks.jar java_lib/

check:
	$(GRADLE) check

dist:
	$(GRADLE) installDist
	
clean:
	$(REBAR) clean
	$(GRADLE) clean

# utils
start: compile
	echo start
	$(ERL) -pa ebin -sname master -config ./sys -eval 'application:start(equartz)'

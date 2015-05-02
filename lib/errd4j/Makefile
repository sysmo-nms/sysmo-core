# Makefile

ERLC ?= erlc -Werror
ERL ?= erl

GRADLE ?= gradle --daemon

.PHONY: compile clean doc start assemble check dist

# define erl targets
ERL_SRC = $(wildcard src/*.erl)
ERL_TARGETS = $(patsubst src/%.erl, ebin/%.beam, $(ERL_SRC))

compile: $(ERL_TARGETS) assemble

# erlang targets
ebin/%.beam: src/%.erl
	$(ERLC) $(EPATH) -o ebin/ src/$*.erl

# java
assemble:
	$(GRADLE) assemble
	cp build/libs/errd4j.jar java_lib/

check:
	$(GRADLE) check

dist:
	$(GRADLE) installDist
	

# utils
start: compile
	$(ERL) -setcookie AAAAAA -pa ebin -sname sysmo -config ./sys -eval 'application:start(errd4j)'

clean:
	$(GRADLE) clean
	rm -f java_lib/errd4j.jar
	rm -f ebin/*.beam
	rm -f erl_crash.dump
	rm -f errd4j.log
	rm -f test.rrd
	rm -f doc/erlang.png
	rm -f doc/*html
	rm -f doc/edoc-info
	rm -f doc/stylesheet.css

doc:
	$(ERL) -noinput -pa ebin -eval 'edoc:application($(APP), ".", [{subpackages, true}, {packages, false}]), init:stop()'

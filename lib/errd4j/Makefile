# Makefile

ERLC ?= erlc -Werror
ERL  ?= erl

.PHONY: compile java-grad clean doc start

# define erl targets
ERL_SRC		= $(wildcard src/*.erl)
ERL_TARGETS	= $(patsubst src/%.erl, ebin/%.beam, $(ERL_SRC))

compile: $(ERL_TARGETS) grad-assemble

# erlang targets
ebin/%.beam: src/%.erl
	$(ERLC) $(EPATH) -o ebin/ src/$*.erl

# java
grad-assemble:
	gradle assemble
	cp build/libs/errd4j.jar java_lib/
	

# utils
start: compile
	$(ERL) -setcookie AAAAAA -pa ebin -sname sysmo -config ./sys -eval 'application:start(errd4j)'

clean:
	gradle clean
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

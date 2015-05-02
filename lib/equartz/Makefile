# Makefile

ERL			?= erl
ERLC		?= erlc -Werror

.PHONY: compile clean doc start assemble check dist

# define erl targets
ERL_SRC		= $(wildcard src/*.erl)
ERL_TARGETS	= $(patsubst src/%.erl, ebin/%.beam, $(ERL_SRC))

compile: $(ERL_TARGETS) assemble

# erlang targets
ebin/%.beam: src/%.erl
	$(ERLC) $(EPATH) -o ebin/ src/$*.erl

# java
assemble:
	gradle assemble
	cp build/libs/equartz.jar java_lib/

check:
	gradle check

dist:
	gradle installDist
	
clean:
	gradle clean
	rm -f java_lib/equartz.jar
	rm -rf java_bin/io
	rm -f ebin/*.beam
	rm -f erl_crash.dump
	rm -f equartz.log
	rm -f doc/erlang.png
	rm -f doc/*html
	rm -f doc/edoc-info
	rm -f doc/stylesheet.css

# utils
start: compile
	echo start
	$(ERL) -pa ebin -sname master -config ./sys -eval 'application:start(equartz)'

# doc
doc:
	$(ERL) -noinput -pa ebin -eval 'edoc:application($(APP), ".", [{subpackages, true}, {packages, false}]), init:stop()'


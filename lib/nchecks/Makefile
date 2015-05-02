# Makefile

ERL  ?= erl
ERLC ?= erlc -Werror

.PHONY: compile clean assemble check dist

# define erl targets
ERL_SRC		= $(wildcard src/*.erl)
ERL_TARGETS	= $(patsubst src/%.erl, ebin/%.beam, $(ERL_SRC))

compile: $(ERL_TARGETS) assemble

ebin/%.beam: src/%.erl $(INCLUDE)
	$(ERLC) $(EPATH) -o ebin/ src/$*.erl

assemble:
	gradle assemble
	cp build/libs/nchecks.jar java_lib/

check:
	gradle check

dist:
	gradle installDist

clean: 
	gradle clean
	rm -f java_lib/nchecks.jar
	rm -f ebin/*.beam
	rm -f *.dump
	rm -f doc/*.html
	rm -f doc/*.png
	rm -f doc/*.css
	rm -f doc/edoc-info
	rm -rf java_bin/io
	rm -f nchecks.log

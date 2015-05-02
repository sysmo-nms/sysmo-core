# Makefile

ERL  ?= erl
ERLC ?= erlc -Werror

.PHONY: compile java-grad clean

# define erl targets
ERL_SRC		= $(wildcard src/*.erl)
ERL_TARGETS	= $(patsubst src/%.erl, ebin/%.beam, $(ERL_SRC))

compile: $(ERL_TARGETS) java-grad

ebin/%.beam: src/%.erl $(INCLUDE)
	$(ERLC) $(EPATH) -o ebin/ src/$*.erl

java-grad:
	gradle build
	cp build/jar/NChecks.jar java_lib/

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

# Makefile

ERL         ?= erl
ERLC        ?= erlc -Werror
EPATH       += -pa ebin -pa ../tracker/ebin

APP         = $(subst ebin/,,$(basename $(wildcard ebin/*.app)))
INCLUDE     = $(wildcard include/*.hrl)

MAIN        = $(wildcard src/*.erl)
MAIN_DST    = $(patsubst src/%.erl, ebin/%.beam, $(MAIN))

compile: $(MAIN_DST)


ebin/%.beam: src/%.erl $(INCLUDE)
	$(ERLC) $(EPATH) -o ebin/ src/$*.erl

clean:
	rm -f ebin/*.beam
	rm -f *.dump
	rm -f doc/*.html
	rm -f doc/*.png
	rm -f doc/*.css
	rm -f doc/edoc-info

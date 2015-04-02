# Makefile

ERL         ?= erl
ERLC        ?= erlc -Werror
EPATH       += -pa ../supercast/ebin -pa ebin

APP             = monitor

MAIN            = $(wildcard src/*.erl)
MAIN_DST        = $(patsubst src/%.erl, ebin/%.beam, $(MAIN))

INCLUDE         = $(wildcard include/*.hrl)

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

# DOCUMENTATION
doc: doc/index.html

doc/index.html: doc/overview.edoc src/*.erl
	@echo "Building documentation..."
	@erl -noinput -pa ebin -eval \
 'edoc:application($(APP), ".", []), init:stop()'

# TEST
test: compile
	@$(ERL) -noinput -pa ebin -eval \
    'eunit:test(monitor, [verbose]), init:stop()'

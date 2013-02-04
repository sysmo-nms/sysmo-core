# Makefile

APP     = probe
ERL     = erl
ERLC   += erlc -Werror
SRC     = $(shell find src -name "*.erl")
ALL_DEST    = $(patsubst src/%.erl, ebin/%.beam, $(SRC))
INCLUDE = $(wildcard include/*.hrl)
EPATH  += -pa ../ifs/ebin -pa ebin

compile: $(ALL_DEST)

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
    'eunit:test(probe, [verbose]), init:stop()'

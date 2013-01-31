# Makefile

APP     = targets
ERL     = erl
ERLC   += erlc -Werror
SRC     = $(shell find src -name "*.erl")
ALL_DEST    = $(patsubst src/%.erl, ebin/%.beam, $(SRC))
BEHAVIOURS  = $(filter ebin/%behaviour.beam, $(ALL_DEST))
OTHER_DEST  = $(filter-out ebin/%behaviour.beam, $(ALL_DEST))
INCLUDE = $(wildcard include/*.hrl)
EPATH  += -pa ../ifs-1.0.1/ebin -pa ebin

compile: $(BEHAVIOURS) $(OTHER_DEST)

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
    'eunit:test(targets, [verbose]), init:stop()'

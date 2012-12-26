# Makefile

EPATH   += -pa ../ifs-0.1.0/ebin
ERLC    = erlc -Werror $(EPATH)
SRC     = $(shell find src -name "*.erl")
DST     = $(patsubst src/%.erl, ebin/%.beam, $(SRC))
APP     = $(subst ebin/,,$(basename $(wildcard ebin/*.app)))

compile: $(DST)

ebin/%.beam: src/%.erl 
	$(ERLC) -o ebin/ src/$*.erl

clean:
	rm -f ebin/*.beam
	rm -f *.dump
	rm -f doc/*.html
	rm -f doc/*.png
	rm -f doc/*.css
	rm -f doc/edoc-info

# TESTS
test: compile
	@erl -noinput -pa ebin -eval \
 'application:start($(APP)), eunit:test(modsrv, [verbose]), init:stop()'

# DOCUMENTATION
doc: doc/index.html

doc/index.html: doc/overview.edoc src/*.erl
	@echo "Building documentation..."
	@erl -noinput -pa ebin -eval \
 'edoc:application($(APP), ".", []), init:stop()'

# Makefile

ERLCC   += erlc -Werror -pa ../ifs-0.1.0/ebin
SRC     = $(shell find src -name "*.erl")
DEST    = $(patsubst src/%.erl, ebin/%.beam, $(SRC))
APP     = $(subst ebin/,,$(basename $(wildcard ebin/*.app)))

compile: $(DEST)

ebin/%.beam: src/%.erl $(INCLUDE)
	$(ERLCC) -o ebin/ src/$*.erl

clean:
	rm -f ebin/*.beam
	rm -f doc/*.html
	rm -f doc/*.png
	rm -f doc/*.css
	rm -f doc/edoc-info
	rm -f erl_crash.dump

# TESTS
test: compile
	@sudo erl -noinput -pa ebin -eval \
 'application:start($(APP)), eunit:test(modsrv, [verbose]), init:stop()'

# DOCUMENTATION
doc: doc/index.html

doc/index.html: doc/overview.edoc src/*.erl
	erl -noinput -pa ebin -eval \
 'edoc:application($(APP), ".", []), init:stop()'


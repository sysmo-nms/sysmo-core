# Makefile

ERLCC   += erlc -Werror -pa ../ifs-0.1.0/ebin
SRC     = $(shell find src -name "*.erl")
DEST    = $(patsubst src/%.erl, ebin/%.beam, $(SRC))

compile: $(DEST)

ebin/%.beam: src/%.erl $(INCLUDE)
	$(ERLCC) -o ebin/ src/$*.erl

clean:
	rm -f ebin/*.beam
	rm -f doc/*
	rm -f erl_crash.dump

doc: doc/index.html

doc/index.html:
	erl -noinput -pa ebin -eval 'edoc:application(emain_emodsrv, ".", []), init:stop()'

test: compile
	@sudo erl -noinput -pa ebin -eval 'application:start(emain_emodsrv), eunit:test(emodsrv, [verbose]), init:stop()'

start: compile
	@sudo erl -pa ebin -eval 'application:start(emain_emodsrv)'

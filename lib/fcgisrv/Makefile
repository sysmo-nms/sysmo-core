ERLC        ?= erlc -Werror
ERL         ?= erl

ERL_SRC		= $(wildcard src/*.erl)
ERL_TARGETS	= $(patsubst src/%.erl, ebin/%.beam, $(ERL_SRC))

.PHONY: clean all cgi

all: $(ERL_TARGETS) cgi

cgi: priv/noctopus-webif

priv/noctopus-webif:
	cd priv; git clone git@github.com:ssbx/noctopus-webif.git; cd noctopus-webif; make

ebin/%.beam: src/%.erl
	$(ERLC) $(EPATH) -o ebin/ src/$*.erl

ebin:
	mkdir ebin

clean:
	rm -f ebin/*.beam
	rm -f erl_crash.dump
	rm -rf priv/noctopus-webif

start: all
	erl -pa ebin -config sys -eval 'application:start(fcgisrv)'

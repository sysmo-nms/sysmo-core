ERLC        ?= erlc -Werror
ERL         ?= erl

ERL_SRC		= $(wildcard src/*.erl)
ERL_TARGETS	= $(patsubst src/%.erl, ebin/%.beam, $(ERL_SRC))

.PHONY: clean all cgi

all: $(ERL_TARGETS) cgi

cgi: priv/sysmo-webif

priv/sysmo-webif:
	cd priv; git clone git@github.com:ssbx/sysmo-webif.git; cd sysmo-webif; make

ebin/%.beam: src/%.erl
	$(ERLC) $(EPATH) -o ebin/ src/$*.erl

ebin:
	mkdir ebin

clean:
	rm -f ebin/*.beam
	rm -f erl_crash.dump

start: all
	erl -pa ebin -config sys -eval 'application:start(fcgisrv)'

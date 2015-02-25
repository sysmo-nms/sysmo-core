ERLC        ?= erlc -Werror
ERL         ?= erl

#ERL     = /cygdrive/c/Program\ Files/erl6.3/bin/erl -smp
#ERLC    = /cygdrive/c/Program\ Files/erl6.3/bin/erlc -Werror

ERL_SRC		= $(wildcard src/*.erl)
ERL_TARGETS	= $(patsubst src/%.erl, ebin/%.beam, $(ERL_SRC))

.PHONY: clean all

all: $(ERL_TARGETS)

ebin/%.beam: src/%.erl
	$(ERLC) $(EPATH) -o ebin/ src/$*.erl

ebin:
	mkdir ebin

clean:
	rm -f ebin/*.beam
	rm -f erl_crash.dump

start: all
	$(ERL) -pa ebin -config sys -eval 'application:start(errd)'

start1:
	$(ERL)

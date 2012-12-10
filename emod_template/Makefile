# Makefile

ERLCC   += erlc -Werror -pa ../ifs-0.1.0/ebin
SRC     = $(shell find src -name "*.erl")
DEST    = $(patsubst src/%.erl, ebin/%.beam, $(SRC))
MIBDEST = $(patsubst priv/mibs/%.mib, priv/snmp/mibs/%.bin, $(MIBSRC))

INCLUDE = $(wildcard include/*.hrl)

compile: $(DEST)

ebin/%.beam: src/%.erl $(INCLUDE)
	$(ERLCC) -o ebin/ src/$*.erl

clean:
	rm -f ebin/*.beam
	rm -f priv/snmp/mibs/*.bin
	rm -f erl_crash.dump

all:
	make

start: compile
	sudo erl -pa ebin -config sys -eval 'application:start(emod_template)'

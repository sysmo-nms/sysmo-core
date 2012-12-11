# Makefile

ERLCC   += erlc -Werror -pa ../ifs-0.1.0/ebin
SNMPC   := snmpc
SRC     = $(shell find src -name "*.erl")
DEST    = $(patsubst src/%.erl, ebin/%.beam, $(SRC))
MIBSRC  = $(shell find priv/mibs -name "*.mib")
MIBDEST = $(patsubst priv/mibs/%.mib, priv/snmp/mibs/%.bin, $(MIBSRC))
PATHS   = -pa ../main_ifs-0.1.0/ebin
INCLUDE = $(wildcard include/*.hrl)

compile: $(DEST)

mib: $(MIBDEST)

priv/snmp/mibs/%.bin: priv/mibs/%.mib
	$(SNMPC) --o priv/snmp/mibs/ priv/mibs/$*.mib

ebin/%.beam: src/%.erl $(INCLUDE)
	$(ERLCC) $(PATHS) -o ebin/ src/$*.erl

clean:
	rm -f ebin/*.beam
	rm -f priv/snmp/mibs/*.bin
	rm -f erl_crash.dump

all:
	make

werr:
	make werr

start: compile
	#sudo erl -pa ebin -config sys -eval 'application:start(snmp), application:start(esnmp)'
	sudo erl -pa ebin -config sys -eval 'application:start(emod_esnmp)'

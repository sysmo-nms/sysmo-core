# Makefile

APP     = esnmp
ERLC   += erlc -Werror
SNMPC  := snmpc
SRC     = $(shell find src -name "*.erl")
DEST    = $(patsubst src/%.erl, ebin/%.beam, $(SRC))
MIBSRC  = $(shell find priv/mibs -name "*.mib")
MIBDEST = $(patsubst priv/mibs/%.mib, priv/snmp/mibs/%.bin, $(MIBSRC))
EPATH  += -pa ../ifs-0.1.0/ebin
INCLUDE = $(wildcard include/*.hrl)

compile: $(DEST)

ebin/%.beam: src/%.erl $(INCLUDE)
	$(ERLC) $(EPATH) -o ebin/ src/$*.erl

clean:
	rm -f ebin/*.beam
	rm -f *.dump
	rm -f doc/*.html
	rm -f doc/*.png
	rm -f doc/*.css
	rm -f doc/edoc-info
	rm -f priv/snmp/mibs/*.bin

# TEST
test:

# DOCUMENTATION
doc: doc/index.html

doc/index.html: doc/overview.edoc src/*.erl
	@echo "Building documentation..."
	@erl -noinput -pa ebin -eval \
 'edoc:application($(APP), ".", []), init:stop()'


## COMPILING MIBS 
#mib: $(MIBDEST)
#
#priv/snmp/mibs/%.bin: priv/mibs/%.mib
#	$(SNMPC) --o priv/snmp/mibs/ priv/mibs/$*.mib
#

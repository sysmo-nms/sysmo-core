# Makefile

ERL         ?= erl
ERLC        ?= erlc -Werror
EPATH       += -pa ../supercast/ebin -pa ebin

APP     = tracker
MAIN            = $(wildcard src/*.erl)
MAIN_DST        = $(patsubst src/%.erl, ebin/%.beam, $(MAIN))

PROBES          = $(wildcard src/probes/*.erl)
PROBES_DST      = $(patsubst src/probes/%.erl, ebin/%.beam, $(PROBES))

INSPECTORS      = $(wildcard src/inspectors/*.erl)
INSPECTORS_DST  = $(patsubst src/inspectors/%.erl, ebin/%.beam, $(INSPECTORS))

LOGGERS         = $(wildcard src/loggers/*.erl)
LOGGERS_DST     = $(patsubst src/loggers/%.erl, ebin/%.beam, $(LOGGERS))

INCLUDE         = $(wildcard include/*.hrl)
PWD             = $(shell pwd)

compile: $(MAIN_DST) $(PROBES_DST) $(INSPECTORS_DST) $(LOGGERS_DST)
    
nagios-plugins: 
	cd priv/nagios/src/nagios-plugins       && \
    ./configure --prefix=$(PWD)/priv/nagios && \
    make                                    && \
    make install
    

ebin/%.beam: src/%.erl $(INCLUDE)
	$(ERLC) $(EPATH) -o ebin/ src/$*.erl

ebin/%.beam: src/probes/%.erl $(INCLUDE)
	$(ERLC) $(EPATH) -o ebin/ src/probes/$*.erl

ebin/%.beam: src/inspectors/%.erl $(INCLUDE)
	$(ERLC) $(EPATH) -o ebin/ src/inspectors/$*.erl

ebin/%.beam: src/loggers/%.erl $(INCLUDE)
	$(ERLC) $(EPATH) -o ebin/ src/loggers/$*.erl

clean-nagios:
	cd priv/nagios/src/nagios-plugins && make uninstall && make distclean

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
    'eunit:test(tracker, [verbose]), init:stop()'

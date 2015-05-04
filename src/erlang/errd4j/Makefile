# Makefile

REBAR ?= rebar

.PHONY: compile clean start


compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

# utils
start: compile
	echo start
	$(ERL) -pa ebin -sname master -config ./sys -eval 'application:start(errd4j)'

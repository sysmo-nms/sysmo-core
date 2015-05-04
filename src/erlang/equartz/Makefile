# Makefile

REBAR ?= rebar

.PHONY: compile clean start


compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

start: compile
	erl -pa ebin -sname master -config ./sys 

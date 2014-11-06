ERL	  ?= erl
ERLC  ?= erlc -Werror
EPATH += -pa ebin

TARGETS = ebin/ini_parser.beam ebin/ini_lexer.beam ebin/ini.beam

all: $(TARGETS)

ebin/ini.beam: src/ini.erl
	$(ERLC) $(EPATH) -o ebin/ src/ini.erl

ebin/ini_lexer.beam: src/ini_lexer.erl
	$(ERLC) $(EPATH) -o ebin/ src/ini_lexer.erl
src/ini_lexer.erl: src/ini_lexer.xrl
	$(ERL) -noinput -eval 'leex:file("src/ini_lexer.xrl"), init:stop()'

ebin/ini_parser.beam: src/ini_parser.erl
	$(ERLC) $(EPATH) -o ebin/ src/ini_parser.erl
src/ini_parser.erl: src/ini_parser.yrl
	$(ERL) -noinput -eval 'yecc:file("src/ini_parser.yrl"), init:stop()'

clean:
	rm -f ebin/*.beam
	rm -f src/ini_parser.erl
	rm -f src/ini_lexer.erl

start:
	$(ERL) -pa ebin

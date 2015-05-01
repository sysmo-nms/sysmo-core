# Makefile


JAVA		?= java
JAVAC		?= javac
ERL			?= erl
ERLC		?= erlc -Werror

# define erl targets
ERL_SRC		= $(wildcard src/*.erl)
ERL_TARGETS	= $(patsubst src/%.erl, ebin/%.beam, $(ERL_SRC))

compile: $(ERL_TARGETS) java_lib/Snmpman.jar 

# erlang targets
ebin/%.beam: src/%.erl
	$(ERLC) $(EPATH) -o ebin/ src/$*.erl

# java
java_lib/Snmpman.jar:
	ant -silent
	cp build/jar/Snmpman.jar java_lib/

doc: doc/index.html

doc/index.html: doc/overview.edoc $(ERL_SRC)
	$(ERL) -noinput -pa ebin -eval 'edoc:application($(APP), ".", [{subpackages, true}, {packages, false}]), init:stop()'


clean:
	ant -silent clean
	rm -f java_lib/Snmpman.jar
	rm -rf java_bin/io
	rm -f ebin/*.beam
	rm -f erl_crash.dump
	rm -f snmpman4j.log
	rm -f doc/erlang.png
	rm -f doc/*html
	rm -f doc/edoc-info
	rm -f doc/stylesheet.css

# utils
start: compile
	$(ERL) -pa ebin -sname master -config ./sys -eval 'application:start(snmpman)'


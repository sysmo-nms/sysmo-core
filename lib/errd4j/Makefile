# Makefile

# define erl targets
ERL_SRC		= $(wildcard src/*.erl)
ERL_TARGETS	= $(patsubst src/%.erl, ebin/%.beam, $(ERL_SRC))

compile: $(ERL_TARGETS) java_lib/Errd4j.jar

# erlang targets
ebin/%.beam: src/%.erl
	$(ERLC) $(EPATH) -o ebin/ src/$*.erl

# java
java_lib/Errd4j.jar: 
	ant -silent
	cp build/jar/Errd4j.jar java_lib/
	
doc: doc/index.html

doc/index.html: doc/overview.edoc $(ERL_SRC)
	$(ERL) -noinput -pa ebin -eval 'edoc:application($(APP), ".", [{subpackages, true}, {packages, false}]), init:stop()'

# utils
start: compile
	$(ERL) -setcookie AAAAAA -pa ebin -sname sysmo -config ./sys -eval 'application:start(errd4j)'

clean:
	ant -silent clean
	rm -f java_lib/Errd4j.jar
	rm -f ebin/*.beam
	rm -f erl_crash.dump
	rm -f errd4j.log
	rm -f test.rrd
	rm -f doc/erlang.png
	rm -f doc/*html
	rm -f doc/edoc-info
	rm -f doc/stylesheet.css


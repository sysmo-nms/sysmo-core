# Makefile


JAVA		?= java
JAVAC		?= javac

# DEBUG
JAVA	= /cygdrive/c/Program\ Files/Java/jdk1.8.0_31/bin/java.exe
JAVAC   = /cygdrive/c/Program\ Files/Java/jdk1.8.0_31/bin/javac.exe
JAVAJAR = /cygdrive/c/Program\ Files/Java/jdk1.8.0_31/bin/jar.exe
ERL     = /cygdrive/c/Program\ Files/erl6.3/bin/erl -smp
ERLC    = /cygdrive/c/Program\ Files/erl6.3/bin/erlc -Werror
# DEBUG END

ERL			?= erl
ERLC		?= erlc -Werror

APP			= errd4j
# define erl targets
ERL_SRC		= $(wildcard src/*.erl)
ERL_TARGETS	= $(patsubst src/%.erl, ebin/%.beam, $(ERL_SRC))

# define java targets
JLIB	= java_lib
JBIN	= java_bin
JSRC	= java_src
JRRD4J	= $(JBIN)/io/sysmo/errd4j/Errd4j.class

JCLASSPATH_TMP  = $(JLIB)/*:$(JBIN)


ifeq ($(CYGW), CYGWIN)
	JCLASSPATH = '$(shell cygpath -awp $(JCLASSPATH_TMP))'
else
	JCLASSPATH = $(JCLASSPATH_TMP)
endif

#DEBUG
JCLASSPATH = '$(shell cygpath -awp $(JCLASSPATH_TMP))'
#DEBUG END

compile: $(ERL_TARGETS) java_lib/Errd4j.jar 

java_lib/Errd4j.jar: $(JRRD4J)
	$(JAVAJAR) cf java_lib/Errd4j.jar -C java_bin io

doc: doc/index.html

doc/index.html: doc/overview.edoc $(ERL_SRC)
	$(ERL) -noinput -pa ebin -eval 'edoc:application($(APP), ".", [{subpackages, true}, {packages, false}]), init:stop()'


test:

clean: clean-doc
	rm -f java_lib/Errd4j.jar
	rm -rf java_bin/io
	rm -f ebin/*.beam
	rm -f erl_crash.dump
	rm -f errd4j.log
	rm -f test.rrd

clean-doc:
	rm -f doc/erlang.png
	rm -f doc/*html
	rm -f doc/edoc-info
	rm -f doc/stylesheet.css

# erlang targets
ebin/%.beam: src/%.erl
	$(ERLC) $(EPATH) -o ebin/ src/$*.erl


# java Errd4j target
$(JRRD4J): $(JSRC)/Errd4j.java
	$(JAVAC) -d $(JBIN) -classpath $(JCLASSPATH) $(JSRC)/Errd4j.java

# utils
start: compile
	$(ERL) -setcookie AAAAAA -pa ebin -sname sysmo -config ./sys -eval 'application:start(errd4j)'


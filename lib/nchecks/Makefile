# Makefile


APP			= sysmo-worker

# DEBUG
ERL     ?= /cygdrive/c/Program\ Files/erl6.3/bin/erl -smp
ERLC    ?= /cygdrive/c/Program\ Files/erl6.3/bin/erlc -Werror
JAVA	?= /cygdrive/c/Program\ Files/Java/jdk1.8.0_31/bin/java.exe
JAVAC   ?= /cygdrive/c/Program\ Files/Java/jdk1.8.0_31/bin/javac.exe
JAVAJAR ?= /cygdrive/c/Program\ Files/Java/jdk1.8.0_31/bin/jar.exe
JAVADOC ?= /cygdrive/c/Program\ Files/Java/jdk1.8.0_31/bin/javadoc.exe
# DEBUG END

# define erl targets
ERL_SRC		= $(wildcard src/*.erl)
ERL_TARGETS	= $(patsubst src/%.erl, ebin/%.beam, $(ERL_SRC))

# define java targets
JLIB		= java_lib
JBIN		= java_bin
JSRC		= java_src
NCHECKS_PKG = io/sysmo/nchecks


compile: $(ERL_TARGETS) java_lib/NChecks.jar

ebin/%.beam: src/%.erl $(INCLUDE)
	$(ERLC) $(EPATH) -o ebin/ src/$*.erl

java_lib/NChecks.jar: build/jar/NChecks.jar
	cp build/jar/NChecks.jar java_lib

build/jar/NChecks.jar:
	ant compile jar

clean: 
	ant clean
	rm -f ebin/*.beam
	rm -f *.dump
	rm -f doc/*.html
	rm -f doc/*.png
	rm -f doc/*.css
	rm -f doc/edoc-info
	rm -rf java_bin/io
	rm -f java_lib/NChecks.jar
	rm -f nchecks.log

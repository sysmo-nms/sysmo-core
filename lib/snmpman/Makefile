# Makefile

JAVA		?= java
JAVAC		?= javac

ERL			?= erl
ERLC		?= erlc -Werror

APP			= snmpman
# define erl targets
ERL_SRC		= $(wildcard src/*.erl)
ERL_TARGETS	= $(patsubst src/%.erl, ebin/%.beam, $(ERL_SRC))

# define java targets
JLIB		= java_lib
JBIN		= java_bin
JSRC		= java_src
JSNMPMAN	= $(JBIN)/io/noctopus/snmpm/Snmpman.class

SNMP4J			= snmp4j-2.3.1.jar
JINTERFACE		= OtpErlang.jar
SNMP4J_DEP		= $(JLIB)/$(SNMP4J)
JINTERFACE_DEP	= $(JLIB)/$(JINTERFACE)
JCLASSPATH		= $(SNMP4J_DEP):$(JINTERFACE_DEP)


compile: $(JSNMPMAN) $(ERL_TARGETS)

doc: doc/index.html

doc/index.html: doc/overview.edoc $(ERL_SRC)
	$(ERL) -noinput -pa ebin -eval 'edoc:application($(APP), ".", [{subpackages, true}, {packages, false}]), init:stop()'


test:

clean: clean-doc
	rm -rf java_bin/io
	rm -f ebin/*.beam
	rm -f erl_crash.dump
	rm -f snmpman4j.log

clean-doc:
	rm -f doc/erlang.png
	rm -f doc/*html
	rm -f doc/edoc-info
	rm -f doc/stylesheet.css

# erlang targets
ebin/%.beam: src/%.erl
	$(ERLC) $(EPATH) -o ebin/ src/$*.erl


# java Snmpman target
$(JSNMPMAN): $(SNMP4J_DEP) $(JINTERFACE_DEP) $(JSRC)/Snmpman.java
	$(JAVAC) -d $(JBIN) -classpath $(JCLASSPATH) $(JSRC)/Snmpman.java

$(SNMP4J_DEP):
	$(error You need $(SNMP4J) to build Snmpman.class. Download it and put it in the ./$(JLIB) directory)

$(JINTERFACE_DEP):
	$(error You need $(JINTERFACE) to build Snmpman.class. Find it and put int in the ./$(JLIB) directory. Only use the version given by your erlang installation. It should be here: $ERLANG_INSTALL_DIR/lib/jinterface-X.X.X/priv/OtpErlang.jar)



# utils
start: compile
	$(ERL) -pa ebin -sname master -config ./sys -eval 'application:start(snmpman)'

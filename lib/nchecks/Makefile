# Makefile

JAVA		?= java
JAVAC		?= javac

ERL 		?= erl
ERLC		?= erlc

APP			= nchecks
# define erl targets
ERL_SRC		= $(wildcard src/*.erl)
ERL_TARGETS	= $(patsubst src/%.erl, ebin/%.beam, $(ERL_SRC))

# define java targets
JLIB		= java_lib
JBIN		= java_bin
JSRC		= java_src
NCHECKS_PKG = io/noctopus/nchecks
NCHECKS_MOD_DIR = $(JBIN)/io/noctopus/nchecks/modules
NCHECKS_SRC = $(wildcard $(JSRC)/Check*.java)
NCHECKS_TARGETS = $(patsubst $(JSRC)/%.java, $(NCHECKS_MOD_DIR)/%.class, $(NCHECKS_SRC))

NCHECKS_DIR = $(JBIN)/io/noctopus/nchecks

NCHECKS_MAIN_SRC = $(JSRC)/NChecks.java
NCHECKS_MAIN_TARGET = $(patsubst $(JSRC)/%.java, $(NCHECKS_DIR)/%.class, $(NCHECKS_MAIN_SRC))

NCHECKS_IF	= $(NCHECK_DIR)/NChecksInterface.class
NCHECKS_IF_SRC = $(JSRC)/NChecksInterface.java
NCHECKS_IF_TARGET = $(patsubst $(JSRC)/%.java, $(NCHECKS_DIR)/%.class, $(NCHECKS_IF_SRC))

NCHECKS_ARG	= $(NCHECK_DIR)/Argument.class
NCHECKS_ARG_SRC = $(JSRC)/Argument.java
NCHECKS_ARG_TARGET = $(patsubst $(JSRC)/%.java, $(NCHECKS_DIR)/%.class, $(NCHECKS_ARG_SRC))

NCHECKS_RPL	= $(NCHECK_DIR)/Reply.class
NCHECKS_RPL_SRC = $(JSRC)/Reply.java
NCHECKS_RPL_TARGET = $(patsubst $(JSRC)/%.java, $(NCHECKS_DIR)/%.class, $(NCHECKS_RPL_SRC))

# define java libs
JINTERFACE		= OtpErlang.jar
JINTERFACE_DEP	= $(JLIB)/$(JINTERFACE)
JCLASSPATH_TMP	= $(JBIN):$(JINTERFACE_DEP)

ifeq ($(CYGW), CYGWIN)
	JCLASSPATH = '$(shell cygpath -awp $(JCLASSPATH_TMP))'
else
	JCLASSPATH = $(JCLASSPATH_TMP)
endif

compile: $(ERL_TARGETS) $(NCHECKS_MAIN_TARGET)

clean: clean-doc
	rm -rf java_bin/io
	rm -f ebin/*.beam
	rm -f erl_crash.dump
	rm -f nchecks.log

clean-doc:
	rm -f doc/erlang.png
	rm -f doc/*html
	rm -f doc/edoc-info
	rm -f doc/stylesheet.css

# erlang targets
ebin/%.beam: src/%.erl
	$(ERLC) $(EPATH) -o ebin/ src/$*.erl

# java targets:

$(NCHECKS_DIR)/NChecks.class: $(JSRC)/NChecks.java $(JINTERFACE_DEP) $(NCHECKS_TARGETS)
	$(JAVAC) -d $(JBIN) -classpath $(JCLASSPATH) $(JSRC)/NChecks.java

$(NCHECKS_IF_TARGET): $(JSRC)/NChecksInterface.java $(NCHECKS_ARG_TARGET) $(NCHECKS_RPL_TARGET)
	$(JAVAC) -d $(JBIN) -classpath $(JBIN) $(JSRC)/NChecksInterface.java

$(NCHECKS_ARG_TARGET): $(JSRC)/Argument.java
	$(JAVAC) -d $(JBIN) -classpath $(JBIN) $(JSRC)/Argument.java

$(NCHECKS_RPL_TARGET): $(JSRC)/Reply.java
	$(JAVAC) -d $(JBIN) -classpath $(JCLASSPATH) $(JSRC)/Reply.java

$(NCHECKS_MOD_DIR)/%.class: java_src/%.java $(NCHECKS_IF_TARGET)
	$(JAVAC) -d $(JBIN) -classpath $(JBIN) $(JSRC)/$*.java

# java dependency libs
$(JINTERFACE_DEP):
	$(error You need $(JINTERFACE) to build NChecks.class. Find it and put int in the ./$(JLIB) directory. Only use the version given by your erlang installation. It should be here: $ERLANG_INSTALL_DIR/lib/jinterface-X.X.X/priv/OtpErlang.jar)

# utils
start: compile
	$(ERL) -pa ebin -sname master -config ./sys -eval 'application:start($(APP))'

# doc
doc: doc/index.html

doc/index.html: doc/overview.edoc $(ERL_SRC)
	$(ERL) -noinput -pa ebin -eval 'edoc:application($(APP), ".", [{subpackages, true}, {packages, false}]), init:stop()'

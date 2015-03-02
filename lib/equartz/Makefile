# Makefile

JAVA		?= java
JAVAC		?= javac

ERL			?= erl
ERLC		?= erlc -Werror

# DEBUG
#ERL     = /cygdrive/c/Program\ Files/erl6.3/bin/erl -smp
#ERLC    = /cygdrive/c/Program\ Files/erl6.3/bin/erlc -Werror
#JAVA	= /cygdrive/c/Program\ Files/Java/jdk1.8.0_31/bin/java.exe
#JAVAC   = /cygdrive/c/Program\ Files/Java/jdk1.8.0_31/bin/javac.exe
#JAVAJAR = /cygdrive/c/Program\ Files/Java/jdk1.8.0_31/bin/jar.exe
# DEBUG END

APP			= equartz
# define erl targets
ERL_SRC		= $(wildcard src/*.erl)
ERL_TARGETS	= $(patsubst src/%.erl, ebin/%.beam, $(ERL_SRC))

# define java targets
JLIB		= java_lib
JBIN		= java_bin
JSRC		= java_src
EQUARTZ_DIR	= $(JBIN)/io/sysmo/equartz
EQUARTZ		= $(EQUARTZ_DIR)/EQuartz.class

ENODE       = $(EQUARTZ_DIR)/EQuartzNode.class
ENODE_IF    = $(EQUARTZ_DIR)/EQuartzMessageHandler.class

# job targets
JOBS_SRC 		= $(wildcard java_src/Job*.java)
JOBS_TARGETS 	= $(patsubst java_src/%.java, $(EQUARTZ_DIR)/%.class, $(JOBS_SRC))

# deps
QUARTZ			= quartz-2.2.1.jar
JINTERFACE		= OtpErlang.jar
QUARTZ_DEP		= $(JLIB)/$(QUARTZ)
JINTERFACE_DEP	= $(JLIB)/$(JINTERFACE)
JCLASSPATH_TMP  = $(QUARTZ_DEP):$(JINTERFACE_DEP):$(JBIN)

ifeq ($(CYGW), CYGWIN)
	JCLASSPATH = '$(shell cygpath -awp $(JCLASSPATH_TMP))'
else
	JCLASSPATH = $(JCLASSPATH_TMP)
endif

# DEBUG
#JCLASSPATH = '$(shell cygpath -awp $(JCLASSPATH_TMP))'
# DEBUG END

compile: $(ERL_TARGETS) java_lib/EQuartz.jar

java_lib/EQuartz.jar: $(ENODE) $(EQUARTZ)
	$(JAVAJAR) cf java_lib/EQuartz.jar -C java_bin io
	
clean: clean-doc
	rm -f java_lib/EQuartz.jar
	rm -rf java_bin/io
	rm -f ebin/*.beam
	rm -f erl_crash.dump
	rm -f equartz.log

clean-doc:
	rm -f doc/erlang.png
	rm -f doc/*html
	rm -f doc/edoc-info
	rm -f doc/stylesheet.css

# erlang targets
ebin/%.beam: src/%.erl
	$(ERLC) $(EPATH) -o ebin/ src/$*.erl

#enode interface target
$(ENODE_IF): $(JSRC)/EQuartzMessageHandler.java
	$(JAVAC) -d $(JBIN) -classpath $(JCLASSPATH) $(JSRC)/EQuartzMessageHandler.java
# enode target
$(ENODE): $(JSRC)/EQuartzNode.java $(ENODE_IF)
	$(JAVAC) -d $(JBIN) -classpath $(JCLASSPATH) $(JSRC)/EQuartzNode.java

# jobs targets
$(EQUARTZ_DIR)/Job%.class: $(JSRC)/Job%.java
	$(JAVAC) -d $(JBIN) -classpath $(JCLASSPATH) $(JSRC)/Job$*.java

# java EQuartz target
$(EQUARTZ): $(QUARTZ_DEP) $(JINTERFACE_DEP) $(JSRC)/EQuartz.java $(JOBS_TARGETS) $(ENODE)
	$(JAVAC) -d $(JBIN) -classpath $(JCLASSPATH) $(JSRC)/EQuartz.java

$(QUARTZ_DEP):
	$(error You need $(QUARTZ) to build EQuartz.class. Download it and put it in the ./$(JLIB) directory)

$(JINTERFACE_DEP):
	$(error You need $(JINTERFACE) to build EQuartz.class. Find it and put int in the ./$(JLIB) directory. Only use the version given by your erlang installation. It should be here: $ERLANG_INSTALL_DIR/lib/jinterface-X.X.X/priv/OtpErlang.jar)



# utils
start: compile
	echo start
	$(ERL) -pa ebin -sname master -config ./sys -eval 'application:start(equartz)'

# doc
doc: doc/index.html

doc/index.html: doc/overview.edoc $(ERL_SRC)
	$(ERL) -noinput -pa ebin -eval 'edoc:application($(APP), ".", [{subpackages, true}, {packages, false}]), init:stop()'


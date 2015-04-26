# Makefile


APP			= sysmo-worker

# DEBUG
ERL     ?= /cygdrive/c/Program\ Files/erl6.3/bin/erl -smp
ERLC    ?= /cygdrive/c/Program\ Files/erl6.3/bin/erlc -Werror
JAVA	?= /cygdrive/c/Program\ Files/Java/jdk1.8.0_31/bin/java.exe
JAVAC   ?= /cygdrive/c/Program\ Files/Java/jdk1.8.0_31/bin/javac.exe
JAVAJAR ?= /cygdrive/c/Program\ Files/Java/jdk1.8.0_31/bin/jar.exe
# DEBUG END

# define erl targets
ERL_SRC		= $(wildcard src/*.erl)
ERL_TARGETS	= $(patsubst src/%.erl, ebin/%.beam, $(ERL_SRC))

# define java targets
JLIB		= java_lib
JBIN		= java_bin
JSRC		= java_src
NCHECKS_PKG = io/sysmo/nchecks

NCHECKS_MOD_DIR = $(JBIN)/io/sysmo/nchecks/checks
NCHECKS_SRC = $(wildcard $(JSRC)/Check*.java)
NCHECKS_TARGETS = $(patsubst $(JSRC)/%.java, $(NCHECKS_MOD_DIR)/%.class, $(NCHECKS_SRC))

NHELPER_MOD_DIR = $(JBIN)/io/sysmo/nchecks/helpers
NHELPER_SRC = $(wildcard $(JSRC)/Helper*.java)
NHELPER_TARGETS = $(patsubst $(JSRC)/%.java, $(NHELPER_MOD_DIR)/%.class, $(NHELPER_SRC))

NCHECKS_DIR = $(JBIN)/io/sysmo/nchecks

NHELPER_IF	= $(NCHECK_DIR)/NHelperInterface.class
NHELPER_IF_SRC = $(JSRC)/NHelperInterface.java
NHELPER_IF_TARGET = $(patsubst $(JSRC)/%.java, $(NCHECKS_DIR)/%.class, $(NHELPER_IF_SRC))

NCHECKS_MAIN_SRC = $(JSRC)/NChecks.java
NCHECKS_MAIN_TARGET = $(patsubst $(JSRC)/%.java, $(NCHECKS_DIR)/%.class, $(NCHECKS_MAIN_SRC))

NCHECKS_IF	= $(NCHECK_DIR)/NChecksInterface.class
NCHECKS_IF_SRC = $(JSRC)/NChecksInterface.java
NCHECKS_IF_TARGET = $(patsubst $(JSRC)/%.java, $(NCHECKS_DIR)/%.class, $(NCHECKS_IF_SRC))

NCHECKS_CONST = $(NCHECK_DIR)/Const.class
NCHECKS_CONST_SRC = $(JSRC)/Const.java
NCHECKS_CONST_TARGET = $(patsubst $(JSRC)/%.java, $(NCHECKS_DIR)/%.class, $(NCHECKS_CONST_SRC))

NCHECKS_QUERY = $(NCHECK_DIR)/Query.class
NCHECKS_QUERY_SRC = $(JSRC)/Query.java
NCHECKS_QUERY_TARGET = $(patsubst $(JSRC)/%.java, $(NCHECKS_DIR)/%.class, $(NCHECKS_QUERY_SRC))


NHELPER_REPLY_IF = $(NCHECK_DIR)/NHelperReply.class
NHELPER_REPLY_IF_SRC = $(JSRC)/NHelperReply.java
NHELPER_REPLY_IF_TARGET = $(patsubst $(JSRC)/%.java, $(NCHECKS_DIR)/%.class, $(NHELPER_REPLY_IF_SRC))

NHELPER_REPLY_SIMPLE = $(NCHECK_DIR)/NHelperSimpleReply.class
NHELPER_REPLY_SIMPLE_SRC = $(JSRC)/NHelperSimpleReply.java
NHELPER_REPLY_SIMPLE_TARGET = $(patsubst $(JSRC)/%.java, $(NCHECKS_DIR)/%.class, $(NHELPER_REPLY_SIMPLE_SRC))

NHELPER_REPLY_TABLE = $(NCHECK_DIR)/NHelperTableReply.class
NHELPER_REPLY_TABLE_SRC = $(JSRC)/NHelperTableReply.java
NHELPER_REPLY_TABLE_TARGET = $(patsubst $(JSRC)/%.java, $(NCHECKS_DIR)/%.class, $(NHELPER_REPLY_TABLE_SRC))



NHELPER_REPLY_TABLE_ROW = $(NCHECK_DIR)/NHelperTableRow.class
NHELPER_REPLY_TABLE_ROW_SRC = $(JSRC)/NHelperTableRow.java
NHELPER_REPLY_TABLE_ROW_TARGET = $(patsubst $(JSRC)/%.java, $(NCHECKS_DIR)/%.class, $(NHELPER_REPLY_TABLE_ROW_SRC))

NHELPER_REPLY_TABLE_ITEM = $(NCHECK_DIR)/NHelperTableItem.class
NHELPER_REPLY_TABLE_ITEM_SRC = $(JSRC)/NHelperTableItem.java
NHELPER_REPLY_TABLE_ITEM_TARGET = $(patsubst $(JSRC)/%.java, $(NCHECKS_DIR)/%.class, $(NHELPER_REPLY_TABLE_ITEM_SRC))


NCHECKS_ARG	= $(NCHECK_DIR)/Argument.class
NCHECKS_ARG_SRC = $(JSRC)/Argument.java
NCHECKS_ARG_TARGET = $(patsubst $(JSRC)/%.java, $(NCHECKS_DIR)/%.class, $(NCHECKS_ARG_SRC))

NCHECKS_RPL	= $(NCHECK_DIR)/Reply.class
NCHECKS_RPL_SRC = $(JSRC)/Reply.java
NCHECKS_RPL_TARGET = $(patsubst $(JSRC)/%.java, $(NCHECKS_DIR)/%.class, $(NCHECKS_RPL_SRC))

NCHECKS_SNMP		= $(NCHECK_DIR)/NChecksSNMP.class
NCHECKS_SNMP_SRC	= $(JSRC)/NChecksSNMP.java
NCHECKS_SNMP_TARGET = $(patsubst $(JSRC)/%.java, $(NCHECKS_DIR)/%.class, $(NCHECKS_SNMP_SRC))

# define java libs
JINTERFACE		= OtpErlang.jar
JINTERFACE_DEP	= $(JLIB)/$(JINTERFACE)
SNMP4J			= snmp4j-2.3.1.jar
SNMP4J_DEP    	= $(JLIB)/$(SNMP4J)

XJSON			= javax.json-1.0.2.jar
XJSON_DEP		= $(JLIB)/$(XJSON)

JCLASSPATH_TMP	= $(JBIN):$(JINTERFACE_DEP):$(SNMP4J_DEP):$(XJSON_DEP)

ifeq ($(CYGW), CYGWIN)
	JCLASSPATH = '$(shell cygpath -awp $(JCLASSPATH_TMP))'
else
	JCLASSPATH = $(JCLASSPATH_TMP)
endif

#DEBUG BEDGIN
JCLASSPATH = '$(shell cygpath -awp $(JCLASSPATH_TMP))'
#DEBUG END


compile: $(ERL_TARGETS) java_lib/NChecks.jar

ebin/%.beam: src/%.erl $(INCLUDE)
	$(ERLC) $(EPATH) -o ebin/ src/$*.erl

java_lib/NChecks.jar: $(NCHECKS_MAIN_TARGET)
	$(JAVAJAR) cf java_lib/NChecks.jar -C java_bin io

clean: 
	rm -f ebin/*.beam
	rm -f *.dump
	rm -f doc/*.html
	rm -f doc/*.png
	rm -f doc/*.css
	rm -f doc/edoc-info
	rm -rf java_bin/io
	rm -f java_lib/NChecks.jar
	rm -f nchecks.log

# nchecks targets:
$(NCHECKS_DIR)/NChecks.class: $(JSRC)/NChecks.java $(NHELPER_TARGETS) $(NCHECKS_TARGETS) $(NCHECKS_SNMP_TARGET) $(NCHECKS_QUERY_TARGET) $(NHELPER_REPLY_IF_TARGET) $(NHELPER_REPLY_IF_TARGET) 
	$(JAVAC) -d $(JBIN) -classpath $(JCLASSPATH) $(JSRC)/NChecks.java

$(NHELPER_IF_TARGET): $(JSRC)/NHelperInterface.java $(NCHECKS_ARG_TARGET) $(NCHECKS_QUERY_TARGET) $(NHELPER_REPLY_IF_TARGET) $(NHELPER_REPLY_IF_TARGET) 
	$(JAVAC) -d $(JBIN) -classpath $(JBIN) $(JSRC)/NHelperInterface.java

$(NCHECKS_IF_TARGET): $(JSRC)/NChecksInterface.java $(NCHECKS_ARG_TARGET) $(NCHECKS_RPL_TARGET) $(NCHECKS_CONST_TARGET) $(NCHECKS_QUERY_TARGET)
	$(JAVAC) -d $(JBIN) -classpath $(JBIN) $(JSRC)/NChecksInterface.java

$(NCHECKS_ARG_TARGET): $(JSRC)/Argument.java
	$(JAVAC) -d $(JBIN) -classpath $(JBIN) $(JSRC)/Argument.java

$(NCHECKS_RPL_TARGET): $(JSRC)/Reply.java
	$(JAVAC) -d $(JBIN) -classpath $(JCLASSPATH) $(JSRC)/Reply.java

$(NHELPER_REPLY_IF_TARGET): $(JSRC)/NHelperReply.java
	$(JAVAC) -d $(JBIN) -classpath $(JCLASSPATH) $(JSRC)/NHelperReply.java

$(NHELPER_REPLY_TABLE_TARGET): $(JSRC)/NHelperTableReply.java $(NHELPER_REPLY_IF_TARGET) $(NHELPER_REPLY_TABLE_ROW_TARGET) $(NHELPER_REPLY_TABLE_ITEM_TARGET) $(NCHECKS_CONST_TARGET)
	$(JAVAC) -d $(JBIN) -classpath $(JCLASSPATH) $(JSRC)/NHelperTableReply.java

$(NHELPER_REPLY_TABLE_ITEM_TARGET): $(JSRC)/NHelperTableItem.java
	$(JAVAC) -d $(JBIN) -classpath $(JCLASSPATH) $(JSRC)/NHelperTableItem.java

$(NHELPER_REPLY_TABLE_ROW_TARGET): $(JSRC)/NHelperTableRow.java $(NHELPER_REPLY_TABLE_ITEM_TARGET)
	$(JAVAC) -d $(JBIN) -classpath $(JCLASSPATH) $(JSRC)/NHelperTableRow.java

$(NHELPER_REPLY_SIMPLE_TARGET): $(JSRC)/NHelperSimpleReply.java $(NHELPER_REPLY_IF_TARGET) $(NCHECKS_CONST_TARGET)
	$(JAVAC) -d $(JBIN) -classpath $(JCLASSPATH) $(JSRC)/NHelperSimpleReply.java

$(NCHECKS_CONST_TARGET): $(JSRC)/Const.java
	$(JAVAC) -d $(JBIN) -classpath $(JCLASSPATH) $(JSRC)/Const.java

$(NCHECKS_QUERY_TARGET): $(JSRC)/Query.java
	$(JAVAC) -d $(JBIN) -classpath $(JCLASSPATH) $(JSRC)/Query.java

$(NCHECKS_SNMP_TARGET): $(JSRC)/NChecksSNMP.java $(NCHECKS_QUERY_TARGET)
	$(JAVAC) -d $(JBIN) -classpath $(JCLASSPATH) $(JSRC)/NChecksSNMP.java

$(NCHECKS_MOD_DIR)/%.class: java_src/%.java $(NCHECKS_IF_TARGET) $(NCHECKS_SNMP_TARGET) $(NCHECKS_QUERY_TARGET)
	$(JAVAC) -d $(JBIN) -classpath $(JCLASSPATH) $(JSRC)/$*.java

$(NHELPER_MOD_DIR)/%.class: java_src/%.java $(NHELPER_IF_TARGET) $(NCHECKS_SNMP_TARGET) $(NCHECKS_QUERY_TARGET) $(NHELPER_REPLY_TABLE_TARGET) $(NHELPER_REPLY_SIMPLE_TARGET) $(NEHLPER_REPLY_TABLE_ROW_TARGET) $(NCHECKS_CONST_TARGET)
	$(JAVAC) -d $(JBIN) -classpath $(JCLASSPATH) $(JSRC)/$*.java

ALLCLASSPATH = '$(shell cygpath -awp $(JCLASSPATH_TMP):java_lib/NChecks.jar)'

test: compile
	$(JAVA) -cp $(ALLCLASSPATH) io.sysmo.nchecks.NChecks --test

# Makefile 

REL_NAME        = enms
MODS            = supercast tracker tlogger_rrd tlogger_text tracker_events


compile:
	@cd lib; make

all: compile pdu_lib test local-release doc

test:
	@cd lib; make test

doc:
	@cd lib; make doc

var-clean:
	rm -rf var/tracker/*/
	rm -f var/snmp/snmpm_config_db
	rm -f var/sasl/sasl.log

rel-clean:
	rm -f $(REL_NAME).script
	rm -f $(REL_NAME).boot

clean: var-clean
	rm -f erl_crash.dump
	rm -f $(REL_NAME).script
	rm -f $(REL_NAME).boot
	rm -f $(REL_NAME).tar.gz
	@cd lib; make clean

    


# Shared includes from IFS
IFS_INCLUDES_DIR    = ./lib/supercast/include
IFS_INCLUDES_SRC    = $(wildcard $(IFS_INCLUDES_DIR)/*.hrl)
IFS_INCLUDES_DST    = $(addprefix ./include/, $(notdir $(IFS_INCLUDES_SRC)))

pdu_lib: $(IFS_INCLUDES_DST)

$(IFS_INCLUDES_DST): ./include/%.hrl: $(IFS_INCLUDES_DIR)/%.hrl
	cp $(IFS_INCLUDES_DIR)/$*.hrl ./include/$*.hrl


# UTILS
export ERL             = /opt/erlang_otp_R16B02/bin/erl
export ERLC            = /opt/erlang_otp_R16B02/bin/erlc -Werror
export ASNC            = /opt/erlang_otp_R16B02/bin/erlc -Werror -bber
MODS_EBIN_DIR	= $(addprefix ./lib/, $(addsuffix /ebin, $(MODS)))
MODS_DEF_FILE	= $(foreach app, $(MODS_EBIN_DIR), $(wildcard $(app)/*.app))
ERL_NMS_PATH	= $(addprefix -pa ,$(MODS_EBIN_DIR))
ERL_REL_COMM    = 'systools:make_script("$(REL_NAME)", [local]), init:stop()'
ERL_REL_COMM2   = '\
systools:make_script("$(REL_NAME)", []), \
systools:make_tar("$(REL_NAME)", [{erts, code:root_dir()}]),\
init:stop()\
'

start: local-release
	@$(ERL) -sname server -boot ./$(REL_NAME) -config ./sys


# RELEASES
release: clean compile tar rel-clean

tar:
	@echo "Generating $(REL_NAME).tar.gz"
	@$(ERL) -noinput $(ERL_NMS_PATH) -eval $(ERL_REL_COMM2)

local-release: compile $(REL_NAME).script 

$(REL_NAME).script: $(MODS_DEF_FILE) $(REL_NAME).rel
	@echo "Generating $(REL_NAME).script and $(REL_NAME).boot files..."
	@$(ERL) -noinput $(ERL_NMS_PATH) -eval $(ERL_REL_COMM)

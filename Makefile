# Makefile 

REL_NAME        = enms
MODS            = ifs esnmp icmp ifs procket tracker activity_logger errd


compile:
	@cd lib; make

all: compile pdu_lib test local-release doc www

test:
	@cd lib; make test

doc:
	@cd lib; make doc

clean:
	rm -f erl_crash.dump
	rm -f $(REL_NAME).script
	rm -f $(REL_NAME).boot
	rm -f stim.pid
	rm -rf www/htdocs/edoc/*
	rm -rf www/htdocs/documentation.html
	@cd lib; make clean

    


# Shared includes from IFS
IFS_INCLUDES_DIR    = ./lib/ifs/include
IFS_INCLUDES_SRC    = $(wildcard $(IFS_INCLUDES_DIR)/*.hrl)
IFS_INCLUDES_DST    = $(addprefix ./include/, $(notdir $(IFS_INCLUDES_SRC)))

pdu_lib: $(IFS_INCLUDES_DST)

$(IFS_INCLUDES_DST): ./include/%.hrl: $(IFS_INCLUDES_DIR)/%.hrl
	cp $(IFS_INCLUDES_DIR)/$*.hrl ./include/$*.hrl


# UTILS
ERL             = erl
MODS_EBIN_DIR	= $(addprefix ./lib/, $(addsuffix /ebin, $(MODS)))
MODS_DEF_FILE	= $(foreach app, $(MODS_EBIN_DIR), $(wildcard $(app)/*.app))
ERL_NMS_PATH	= $(addprefix -pa ,$(MODS_EBIN_DIR))
ERL_REL_COMM    = 'systools:make_script("$(REL_NAME)", [local]), init:stop()'

start: local-release
	@sudo $(ERL) -sname server -boot ./$(REL_NAME) -config ./sys




# RELEASES
local-release: compile $(REL_NAME).script 

$(REL_NAME).script: $(MODS_DEF_FILE) $(REL_NAME).rel
	@echo "Generating $(REL_NAME).script and $(REL_NAME).boot files..."
	@$(ERL) -noinput $(ERL_NMS_PATH) -eval $(ERL_REL_COMM)



# PRIVATE UTILS
commit:
	@echo -n "coment: "; read COMENT; \
	git add -A; git commit -m "$$COMENT"

www: doc
	@for i in $(MODS); do cp -r lib/$$i/doc www/htdocs/edoc/$$i; done
	@ ./www/gen-htdoc $(MODS)

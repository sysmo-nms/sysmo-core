# Makefile 

REL_NAME        = enms
MODS			= main_ifs main_modsrv mod_esnmp 

MODS_VER		= $(foreach app, $(MODS), $(wildcard $(app)-*))

compile: recurse pdu_lib

recurse:
	@for i in $(MODS_VER); do cd $$i; make compile; cd ../; done

test:
	@for i in $(MODS_VER); do cd $$i; make test; cd ../; done

doc:
	@for i in $(MODS_VER); do cd $$i; make doc; cd ../; done

clean:
	rm -f erl_crash.dump
	rm -f $(REL_NAME).script
	rm -f $(REL_NAME).boot
	@for i in $(MODS_VER); do cd $$i; make clean; cd ../; done

    


# Shared includes from IFS
IFS_DIR         = $(filter main_ifs%, $(MODS_VER))
IFS_INCLUDES_DIR    = $(addsuffix /include, $(addprefix ./, $(IFS_DIR)))
IFS_INCLUDES_SRC    = $(wildcard $(IFS_INCLUDES_DIR)/*.hrl)
IFS_INCLUDES_DST    = $(addprefix ./include/, $(notdir $(IFS_INCLUDES_SRC)))

pdu_lib: $(IFS_INCLUDES_DST)

$(IFS_INCLUDES_DST): ./include/%.hrl: $(IFS_INCLUDES_DIR)/%.hrl
	@cp $(IFS_INCLUDES_DIR)/$*.hrl ./include/$*.hrl

	

# UTILS
ERL             = erl
MODS_EBIN_DIR	= $(addprefix ./, $(addsuffix /ebin, $(MODS_VER)))
MODS_DEF_FILE	= $(foreach app, $(MODS_EBIN_DIR), $(wildcard $(app)/*.app))
ERL_NMS_PATH	= $(addprefix -pa ,$(MODS_EBIN_DIR))
ERL_REL_COMM    = 'systools:make_script("$(REL_NAME)", [local]), init:stop()'

start: local-release
	sudo $(ERL) -sname server -boot ./$(REL_NAME) -config ./sys

clifs: compile
	@$(ERL) $(ERL_NMS_PATH) -sname "client" -eval 'clifs:start_link()'




# RELEASES
local-release: compile $(REL_NAME).script 

$(REL_NAME).script: $(MODS_DEF_FILE)
	@echo "Generating $(REL_NAME).script and $(REL_NAME).boot files..."
	@$(ERL) -noinput $(ERL_NMS_PATH) -eval $(ERL_REL_COMM)



# PRIVATE UTILS
commit: clean
	@echo -n "coment: "; read COMENT; \
	git add -A; git commit -m "$$COMENT"

push:
	git push -u origin master

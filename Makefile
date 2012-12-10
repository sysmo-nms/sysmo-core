# Makefile 
NMS_NAME		= nms_project
MODS			= main_ifs main_emodsrv emod_esnmp
ERL				= erl
ERLC			= erlc -Werror
SUDO_ERL		= sudo erl

MODS_VER		= $(foreach app, $(MODS), $(wildcard $(app)-*))
MODS_EBIN_DIR	= $(addprefix ./, $(addsuffix /ebin, $(MODS_VER)))
MODS_DEF_FILE	= $(foreach app, $(MODS_EBIN_DIR), $(wildcard $(app)/*.app))

# MAKE NMSEASE s appui sur les $(app).app enms.rel et le fichier enms.script
ERL_NMS_PATH		= $(addprefix -pa ,$(MODS_EBIN_DIR))
ERL_START           = 'application:start(mnesia), application:start(crypto), application:start(public_key), \
	                        application:start(ssl), application:start(snmp), application:start(main_modsrv), \
                                application:start(main_ifs), application:start(emod_esnmp)'
ERL_START_STIM      = 'application:start(crypto), application:start(public_key), \
	                    application:start(ssl), application:start(nms_core), mod_test1:start_link(), \
                        mod_test1:stim_updates()'

compile:
	@for i in $(MODS_VER); do cd $$i; make ERLC="$(ERLC)"; cd ../; done

start: compile
	@sudo $(ERL) -sname $(NMS_NAME) -config sys $(ERL_NMS_PATH) -eval $(ERL_START)

start-stim: compile
	@$(ERL) -sname $(NMS_NAME) -config sys $(ERL_NMS_PATH) -eval $(ERL_START_STIM)

client: compile
	@echo -n "user name: "; read UName; export UName; \
	echo -n "pass word: "; read UPass; export UPass; \
	$(ERL) $(ERL_NMS_PATH) -run asncli -username $$UName -userpass $$UPass

adm: compile
	$(ERL) $(ERL_NMS_PATH) -run asncli -username admuser -userpass passwd
	
doc: compile
	@for i in $(MODS_VER); do cd $$i; make doc ERLC="$(ERLC)"; cd ../; done
    
clifs: compile
	$(ERL) $(ERL_NMS_PATH) -eval 'clifs:start_link()'

push: clean
	echo -n "coment: "; read COMENT; \
	git add -A; git commit -m "$$COMENT"; git push -u origin master

clean:
	rm -rf erl_crash.dump MnesiaCore.* *.pid
	rm -f SLASH/var/lib/manager/* SLASH/var/lib/agent/*
	@for i in $(MODS_VER); do cd $$i; make clean; cd ../; done

werr: clean
	@for i in $(MODS_VER); do cd $$i; make ERLC="$(WERLC)"; cd ../; done

# Makefile 

MODS			= main_ifs main_modsrv mod_esnmp 

MODS_VER		= $(foreach app, $(MODS), $(wildcard $(app)-*))
MODS_EBIN_DIR	= $(addprefix ./, $(addsuffix /ebin, $(MODS_VER)))
MODS_DEF_FILE	= $(foreach app, $(MODS_EBIN_DIR), $(wildcard $(app)/*.app))

compile:
	@for i in $(MODS_VER); do cd $$i; make compile; cd ../; done

test:
	@for i in $(MODS_VER); do cd $$i; make test; cd ../; done

doc:
	@for i in $(MODS_VER); do cd $$i; make doc; cd ../; done

clean:
	rm -f erl_crash.dump
	@for i in $(MODS_VER); do cd $$i; make clean; cd ../; done

# UTILS
ERL             = erl
ERL_NMS_PATH	= $(addprefix -pa ,$(MODS_EBIN_DIR))
ERL_START       = 'application:start(mnesia), application:start(crypto), \
    application:start(public_key), application:start(ssl), \
    application:start(snmp), application:start(main_modsrv), \
    application:start(main_ifs), application:start(mod_esnmp)'
start: compile
	@sudo $(ERL) -sname server \
        -config sys $(ERL_NMS_PATH) -eval $(ERL_START)

clifs: compile
	@$(ERL) $(ERL_NMS_PATH) -sname "client" -eval 'clifs:start_link()'

push: clean
	@echo -n "coment: "; read COMENT; \
	git add -A; git commit -m "$$COMENT"; git push -u origin master

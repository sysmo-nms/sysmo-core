# Makefile 
export MAKE        = /usr/bin/make
export REL_NAME    = noctopus
export REL_VERSION = 0.2.1
export MODS = supercast monitor errdtools text_logger snmpman noctopus


.PHONY: compile test doc clean var-clean rel-clean start \
	unix-release unix-local-release windows-release windows-local-release

compile:
	$(MAKE) -C lib

test:
	$(MAKE) -C lib test

doc:
	$(MAKE) -C lib doc

clean: var-clean rel-clean
	rm -f erl_crash.dump
	rm -f MnesiaCore.*
	$(MAKE) -C lib clean

var-clean:
	rm -rf var/monitor/*/
	rm -rf var/monitor/targets.dets
	rm -rf var/monitor_events
	rm -f var/snmp/snmpm_config_db
	rm -f var/log/*.log
	rm -f var/mnesia/*.LOG
	rm -f var/mnesia/*.DAT
	rm -f var/mnesia/*.DCD
	rm -f var/mnesia/*.DCL
	rm -f var/engindID.conf

rel-clean:
	rm -f $(REL_NAME).script
	rm -f $(REL_NAME).boot
	rm -f sys.config
	rm -f var/httpd/8080_props.conf
	rm -f $(REL_NAME).tar
	rm -f $(REL_NAME)-$(REL_VERSION).tar.gz
	rm -rf $(REL_NAME)-$(REL_VERSION).win32






######################
# RELEASES UTILITIES #
######################
UNAME = $(shell uname)
CYGW  = $(findstring CYGWIN, $(UNAME))
ifeq ($(CYGW), CYGWIN)
    RELEASE        = windows-release
    LOCAL_RELEASE  = windows-local-release
    export ERL     = /cygdrive/c/Program\ Files/erl5.10.4/bin/erl
    export ERLC    = /cygdrive/c/Program\ Files/erl5.10.4/bin/erlc -Werror
    export ASNC    = /cygdrive/c/Program\ Files/erl5.10.4/bin/erlc -Werror -bber
else
    RELEASE        = unix-release
    LOCAL_RELEASE  = unix-local-release
    export ERL     = /opt/erlang_otp_17.1/bin/erl
    export ERLC    = /opt/erlang_otp_17.1/bin/erlc -Werror
    export ASNC    = /opt/erlang_otp_17.1/bin/erlc -Werror -bber
endif

MODS_EBIN_DIR      = $(addprefix ./lib/, $(addsuffix /ebin, $(MODS)))
MODS_DEF_FILE      = $(foreach app, $(MODS_EBIN_DIR), $(wildcard $(app)/*.app))
ERL_NMS_PATH       = $(addprefix -pa ,$(MODS_EBIN_DIR))
ERL_REL_COMM       = '\
    systools:make_script("$(REL_NAME)", [local]),\
    init:stop()\
'
ERL_REL_COMM2   = '\
    systools:make_script("$(REL_NAME)", []), \
    systools:make_tar("$(REL_NAME)", [{erts, code:root_dir()}]),\
    init:stop()\
'

start: rel-clean $(LOCAL_RELEASE)
	@$(ERL) -sname master -boot ./$(REL_NAME) -config ./sys

release: $(RELEASE)

$(REL_NAME).script: $(MODS_DEF_FILE) $(REL_NAME).rel
	@echo "Generating $(REL_NAME).script and $(REL_NAME).boot files..."
	@$(ERL) -noinput $(ERL_NMS_PATH) -eval $(ERL_REL_COMM)


TMP_DIR     = /tmp/nms_tar_dir
WIN_TMP_DIR = C:\\cygwin\\tmp\\nms_tar_dir
ERL_UNTAR   = '\
    File = "$(REL_NAME).tar", \
    erl_tar:extract(File, [{cwd, "$(WIN_TMP_DIR)"}]), \
    init:stop() \
'

##########################
# WINDOWS RELEASES BEGIN #
##########################
windows-local-release: compile $(REL_NAME).script
	cp release_tools/local/sys.config.dev ./sys.config
	cp release_tools/local/8080_props.conf.dev ./var/httpd/8080_props.conf
	chmod -w sys.config
	chmod -w var/httpd/8080_props.conf


windows-release: var-clean rel-clean compile
	@echo "Generating $(REL_NAME)-$(REL_VERSION).win32 directory"
	@$(ERL) -noinput $(ERL_NMS_PATH) -eval $(ERL_REL_COMM2)
	@rm -rf $(TMP_DIR)
	@mkdir  $(TMP_DIR)
	@gzip -d $(REL_NAME).tar.gz
	@rm -f   $(REL_NAME).tar.gz
	@$(ERL) -noinput -eval $(ERL_UNTAR)
	@cp -R var $(TMP_DIR)/
	@mkdir $(TMP_DIR)/bin
	@cp release_tools/win32/noctopus.bat.src $(TMP_DIR)/bin/noctopus.bat
	@cp release_tools/win32/register_noctopus_nt-service.bat.src $(TMP_DIR)/bin/register_noctopus_nt-service.bat
	@cp release_tools/win32/erl.ini.src      $(TMP_DIR)/erts-5.10.4/bin/erl.ini.src
	@cp release_tools/sys.config.src   $(TMP_DIR)/releases/$(REL_VERSION)/sys.config
	@cp release_tools/8080_props.conf.src  $(TMP_DIR)/var/httpd/8080_props.conf
	@mkdir $(TMP_DIR)/cfg
	@touch $(TMP_DIR)/cfg/monitor.conf
	@cp -r $(TMP_DIR) $(REL_NAME)-$(REL_VERSION).win32
########################
# WINDOWS RELEASES END #
########################







#######################
# UNIX RELEASES BEGIN #
#######################
unix-local-release: compile $(REL_NAME).script
	cp release_tools/local/sys.config.dev sys.config
	cp release_tools/local/8080_props.conf.dev var/httpd/8080_props.conf
	chmod -w sys.config
	chmod -w var/httpd/8080_props.conf

unix-release: var-clean rel-clean compile
	@echo "Generating $(REL_NAME)-$(REL_VERSION).tar.gz"
	@$(ERL) -noinput $(ERL_NMS_PATH) -eval $(ERL_REL_COMM2)
	@rm -rf $(TMP_DIR)
	@mkdir $(TMP_DIR)
	@tar xzf $(REL_NAME).tar.gz -C $(TMP_DIR)
	@rm -f $(REL_NAME).tar.gz
	@cp -R var $(TMP_DIR)/
	@mkdir $(TMP_DIR)/bin
	@cp release_tools/unix/noctopus $(TMP_DIR)/bin/
	@cp release_tools/unix/install $(TMP_DIR)
	@cp release_tools/sys.config.src $(TMP_DIR)/releases/$(REL_VERSION)/
	@cp release_tools/8080_props.conf.src $(TMP_DIR)/var/httpd/
	@mkdir $(TMP_DIR)/cfg
	@touch $(TMP_DIR)/cfg/monitor.conf
	@tar -czf $(REL_NAME)-$(REL_VERSION).tar.gz -C $(TMP_DIR) .
#####################
# UNIX RELEASES END #
#####################

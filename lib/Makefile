# Makefile 


compile: recurse external_make

all: compile doc test local-release www

recurse: snmpman
	@for i in $(MODS); do cd $$i; $(MAKE) compile; cd ../; done

test: snmpman
	@for i in $(MODS); do cd $$i; $(MAKE) test;  cd ../; done

doc: snmpman
	@for i in $(MODS); do cd $$i; $(MAKE) doc;  cd ../; done

clean: external_clean
	@for i in $(MODS); do cd $$i; $(MAKE) clean;  cd ../; done



EXTERNAL = nchecks
external_update: snmpman
	cd $(EXTERNAL); git pull

external_make: $(EXTERNAL) snmpman
	@cd $(EXTERNAL); make

external_clean: $(EXTERNAL)
	@cd $(EXTERNAL); make clean

nchecks:
	@git clone git@github.com:ssbx/nchecks.git

snmpman:
	@git clone git@github.com:ssbx/snmpman.git

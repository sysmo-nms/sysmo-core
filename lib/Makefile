.PHONY: compile test doc clean

compile: $(MODS)
	@for i in $(MODS); do $(MAKE) -C $$i; done

test: $(MODS)
	@for i in $(MODS); do $(MAKE) -C $$i test; done

doc: $(MODS)
	@for i in $(MODS); do $(MAKE) -C $$i doc; done

clean: $(MODS)
	@for i in $(MODS); do $(MAKE) -C $$i clean; done


nchecks:
	@git clone git@github.com:ssbx/nchecks.git

snmpman:
	@git clone git@github.com:ssbx/snmpman.git

errdtools:
	@git clone git@github.com:ssbx/errdtools.git

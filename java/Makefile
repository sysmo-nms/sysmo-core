.PHONY: compile test doc clean
MODS = errd4j equartz snmpman nchecks

compile: $(MODS)
	@for i in $(MODS); do $(MAKE) -C $$i; done

test: $(MODS)
	@for i in $(MODS); do $(MAKE) -C $$i test; done

doc: $(MODS)
	@for i in $(MODS); do $(MAKE) -C $$i doc; done

clean: $(MODS)
	@for i in $(MODS); do $(MAKE) -C $$i clean; done

check: $(MODS)
	@for i in $(MODS); do $(MAKE) -C $$i check; done

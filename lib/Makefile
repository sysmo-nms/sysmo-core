.PHONY: compile test doc clean

compile: $(MODS)
	@for i in $(MODS); do $(MAKE) -C $$i; done

test: $(MODS)
	@for i in $(MODS); do $(MAKE) -C $$i test; done

doc: $(MODS)
	@for i in $(MODS); do $(MAKE) -C $$i doc; done

clean: $(MODS)
	@for i in $(MODS); do $(MAKE) -C $$i clean; done

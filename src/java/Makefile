.PHONY: compile test doc clean

export GRADLE_OPTS = \
 -Dorg.gradle.daemon=true \
 -Dorg.gradle.no-search-upwards=true \
 -Dorg.gradle.configure-on-demand=true

MODS = errd4j equartz snmpman nchecks

compile: $(MODS)
	@for i in $(MODS); do $(MAKE) -C $$i; done

test: $(MODS)
	@for i in $(MODS); do $(MAKE) -C $$i test; done

doc: $(MODS)
	@for i in $(MODS); do $(MAKE) -C $$i doc; done

dist: $(MODS)
	@for i in $(MODS); do $(MAKE) -C $$i dist; done

clean: $(MODS)
	@for i in $(MODS); do $(MAKE) -C $$i clean; done

check: $(MODS)
	@for i in $(MODS); do $(MAKE) -C $$i check; done

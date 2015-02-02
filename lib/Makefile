.PHONY: compile test doc clean

# yaws special begin
REQUIRE_YAWS = yaws/ebin/yaws.beam
MODS2 = $(filter-out yaws, $(MODS))
# yaws special end

compile: $(MODS2) $(REQUIRE_YAWS)
	@for i in $(MODS2); do $(MAKE) -C $$i; done

test: $(MODS2)
	@for i in $(MODS2); do $(MAKE) -C $$i test; done

doc: $(MODS2)
	@for i in $(MODS2); do $(MAKE) -C $$i doc; done

clean: $(MODS2)
	@for i in $(MODS2); do $(MAKE) -C $$i clean; done

# yaws special begin
$(REQUIRE_YAWS): yaws
$(REQUIRE_YAWS): ERLC = $(ERLCY)
$(REQUIRE_YAWS):
	cd yaws; \
		git checkout tags/yaws-1.98; \
		autoreconf -fi; \
		./configure --disable-pam; \
		make

yaws: 
	@git clone git@github.com:klacke/yaws.git

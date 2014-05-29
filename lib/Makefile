# Makefile 


compile: recurse external_make

all: compile doc test local-release www

recurse:
	@for i in $(MODS); do cd $$i; $(MAKE) compile; cd ../; done

test:
	@for i in $(MODS); do cd $$i; $(MAKE) test;  cd ../; done

doc:
	@for i in $(MODS); do cd $$i; $(MAKE) doc;  cd ../; done

clean: external_clean
	@for i in $(MODS); do cd $$i; $(MAKE) clean;  cd ../; done



EXTERNAL = noctopus-checks
external_make: $(EXTERNAL)
	@cd $(EXTERNAL); make

external_clean: $(EXTERNAL)
	@cd $(EXTERNAL); make clean

noctopus-checks:
	@git clone git@github.com:ssbx/noctopus-checks.git

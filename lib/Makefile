# Makefile 

EXTERNAL = noctopus-checks/bin

compile: recurse

all: compile doc test local-release www

recurse: externals
	@for i in $(MODS); do cd $$i; $(MAKE) compile; cd ../; done

test:
	@for i in $(MODS); do cd $$i; $(MAKE) test;  cd ../; done

doc:
	@for i in $(MODS); do cd $$i; $(MAKE) doc;  cd ../; done

clean:
	@for i in $(MODS); do cd $$i; $(MAKE) clean;  cd ../; done

externals: $(EXTERNAL)

noctopus-checks/bin: noctopus-checks
	cd noctopus-checks; make

noctopus-checks:
	@git clone git@github.com:ssbx/noctopus-checks.git

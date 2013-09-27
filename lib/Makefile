# Makefile 

REL_NAME        = enms
MODS            = supercast tracker errd erlexec

compile: recurse

all: compile doc test local-release www

recurse:
	@for i in $(MODS); do cd $$i; make compile; cd ../; done

test:
	@for i in $(MODS); do cd $$i; make test; cd ../; done

doc:
	@for i in $(MODS); do cd $$i; make doc; cd ../; done

clean:
	@for i in $(MODS); do cd $$i; make clean; cd ../; done


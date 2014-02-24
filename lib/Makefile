# Makefile 

compile: recurse

all: compile doc test local-release www

recurse: externals_compile
	@for i in $(MODS); do cd $$i; make compile; cd ../; done

test:
	@for i in $(MODS); do cd $$i; make test; cd ../; done

doc:
	@for i in $(MODS); do cd $$i; make doc; cd ../; done

clean: externals_clean
	@for i in $(MODS); do cd $$i; make clean; cd ../; done

externals_compile:
	@for i in $(REBAR); do cd $$i; rebar compile; cd ../; done

externals_clean:
	@for i in $(REBAR); do cd $$i; rebar clean; cd ../; done

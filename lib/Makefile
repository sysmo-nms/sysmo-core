# Makefile 

EXTERNAL = go-check/build

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

go-check/build: go-check
	cd go-check; make

go-check:
	@git clone https://github.com/ssbx/go-check.git


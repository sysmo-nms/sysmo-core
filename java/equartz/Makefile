# Makefile

GRADLE ?= gradle

.PHONY: compile clean doc start check dist


compile: 
	$(GRADLE) classes

check:
	$(GRADLE) check

dist:
	$(GRADLE) installDist
	
clean:
	$(GRADLE) clean

run: dist
	./build/install/equartz/bin/equartz


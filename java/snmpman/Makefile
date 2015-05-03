# Makefile

GRADLE ?= gradle --daemon

.PHONY: compile clean doc start check dist


compile: 
	$(GRADLE) assemble

check:
	$(GRADLE) check

dist:
	$(GRADLE) installDist
	
clean:
	$(GRADLE) clean

run:
	$(GRADLE) run

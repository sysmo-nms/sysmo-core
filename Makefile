export MODS = supercast monitor errd4j snmpman nchecks equartz pping 

compile:
	$(MAKE) -C lib
	rebar compile
	
test:
	$(MAKE) -C lib test
	rebar -r test

doc:
	$(MAKE) -C lib doc
	rebar doc

clean:
	$(MAKE) -C lib clean
	rebar clean

rel: compile
	cd rel; rebar generate


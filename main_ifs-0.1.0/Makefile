# Makefile

NMS_PDU     = ebin/NmsPDU.beam ebin/ModIf.beam ebin/ModEsnmp.beam
ERLC        := erlc
ASNC        := erlc -bber_bin +optimize +nif
ERL         := erl
SRC         = $(shell find src -name "*.erl")
DEST        = $(patsubst src/%.erl, ebin/%.beam, $(SRC))
APP         = $(subst ebin/,,$(basename $(wildcard ebin/*.app)))
INCLUDE     = $(wildcard ../include/*.hrl)
BEHAVIOUR   = ebin/beha_ifs_auth.beam ebin/beha_ifs_module.beam ebin/beha_ifs_encoder.beam

compile: $(BEHAVIOUR) $(DEST) $(NMS_PDU) 

ebin/%.beam: src/%.erl $(INCLUDE)
	$(ERLC) -pa ebin -o ebin/ src/$*.erl

# ASN PDU MODULES
ebin/NmsPDU.beam: priv/asn1/NmsPDU.asn $(MODS_BEAM)
	$(ASNC) -o priv/asn1/build priv/asn1/NmsPDU.asn
	@cp priv/asn1/build/NmsPDU.beam ebin

ebin/ModIf.beam: priv/asn1/ModIf.asn 
	$(ASNC) -o priv/asn1/build priv/asn1/ModIf.asn
	@cp priv/asn1/build/ModIf.beam ebin
	@cp priv/asn1/build/ModIf.hrl  include

ebin/ModEsnmp.beam: priv/asn1/ModEsnmp.asn
	$(ASNC) -o priv/asn1/build priv/asn1/ModEsnmp.asn
	@cp priv/asn1/build/ModEsnmp.beam ebin


# UTILITIES
start: compile
	@$(ERL) -pa ebin -pa pdu -config sys -eval 'application:start(crypto), application:start(public_key), \
                                                application:start(ssl), application:start(nms_core)'

client: compile
	@echo -n "user name: "; read UName; export UName; \
	echo -n "pass word: "; read UPass; export UPass; \
	$(ERL) -pa ebin -pa pdu -run asncli -username $$UName -userpass $$UPass

clean:
	rm -rf ebin/*.beam
	rm -rf erl_crash.dump
	rm -rf doc/*.html
	rm -rf doc/*.png
	rm -rf doc/*.css
	rm -rf doc/edoc-info
	rm -rf priv/asn1/build/*



# DOCUMENTATION
doc: doc/index.html

doc/index.html: doc/overview.edoc src/*.erl
	erl -noinput -pa ebin -eval \
 'edoc:application($(APP), ".", []), init:stop()'

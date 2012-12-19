# Makefile

NMS_PDU     = ebin/NmsPDU.beam ebin/ModIf.beam ebin/ModEsnmp.beam
EPATH      += -pa ebin
ERLC        = erlc -Werror
ASNC        = erlc -Werror -bber_bin +optimize +nif
ERL         = erl
SRC         = $(shell find src -name "*.erl")
DEST        = $(patsubst src/%.erl, ebin/%.beam, $(SRC))
APP         = $(subst ebin/,,$(basename $(wildcard ebin/*.app)))
INCLUDE     = $(wildcard ../include/*.hrl)
BEHAVIOUR   = ebin/beha_ifs_auth.beam ebin/beha_ifs_module.beam \
                ebin/beha_ifs_encoder.beam

compile: $(BEHAVIOUR) $(DEST) $(NMS_PDU) 

ebin/%.beam: src/%.erl $(INCLUDE)
	$(ERLC) $(EPATH) -o ebin/ src/$*.erl

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

clean:
	rm -f ebin/*.beam
	rm -f doc/*.html
	rm -f doc/*.png
	rm -f doc/*.css
	rm -f doc/edoc-info
	rm -f priv/asn1/build/*

# TEST
test:

# DOCUMENTATION
doc: doc/index.html

doc/index.html: doc/overview.edoc src/*.erl
	@echo "Building documentation..."
	@$(ERL) -noinput -pa ebin -eval \
 'edoc:application($(APP), ".", []), init:stop()'

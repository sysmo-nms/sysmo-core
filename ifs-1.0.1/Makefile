# Makefile

EPATH      += -pa ebin
ERLC        = erlc -Werror
ASNC        = erlc -Werror -bber_bin +optimize +nif
ERL         = erl

APP         = $(subst ebin/,,$(basename $(wildcard ebin/*.app)))
INCLUDE     = $(wildcard ../include/*.hrl)

SRC         = $(shell find src -name "*.erl")
ALL_DEST    = $(patsubst src/%.erl, ebin/%.beam, $(SRC))
BEHAVIOURS  = $(filter      ebin/beha%.beam, $(ALL_DEST))
OTHER_DEST  = $(filter-out  ebin/beha%.beam, $(ALL_DEST))

compile: $(BEHAVIOURS) $(OTHER_DEST) pdu

$(BEHAVIOURS): ebin/%.beam: src/%.erl
	$(ERLC) $(EPATH) -o ebin/ src/$*.erl

$(OTHER_DEST): ebin/%.beam: src/%.erl
	$(ERLC) $(EPATH) -o ebin/ src/$*.erl

clean:
	rm -f ebin/*.beam
	rm -f *.dump
	rm -f doc/*.html
	rm -f doc/*.png
	rm -f doc/*.css
	rm -f doc/edoc-info
	rm -f priv/asn1/build/*

# ASN PDUs
ASN_DEFS    = $(shell find priv/asn1 -name "*.asn")
ASN_DEST    = $(patsubst priv/asn1/%.asn, ebin/%.beam, $(ASN_DEFS))

pdu: $(ASN_DEST)

$(ASN_DEST): ebin/%.beam: priv/asn1/%.asn
	$(ASNC) -o priv/asn1/build priv/asn1/$*.asn
	@cp priv/asn1/build/$*.beam ebin
	@cp priv/asn1/build/*.hrl include


# TEST
test: compile

# DOCUMENTATION
doc: doc/index.html

doc/index.html: doc/overview.edoc src/*.erl
	@echo "Building documentation..."
	@$(ERL) -noinput -pa ebin -eval \
 'edoc:application($(APP), ".", []), init:stop()'

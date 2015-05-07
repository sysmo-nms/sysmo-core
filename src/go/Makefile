GO_COMP ?= go
PPING_OUT ?= pping

$(PPING_OUT): pping.go
	$(GO_COMP) build -o $(PPING_OUT) pping.go

clean:
	rm -f $(PPING_OUT)

#chown root:root pping; chmod +s pping

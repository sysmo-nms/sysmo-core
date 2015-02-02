UNAME = $(shell uname)
CYGW  = $(findstring CYGWIN, $(UNAME))
ifeq ($(CYGW), CYGWIN)
	GO = /cygdrive/c/Go/bin/go.exe
	EXTENTION = .exe
else
	GO = go
endif


SRC_DIR  = .
BIN_DIR  = .
GO_BUILD = $(GO) build

PPING_SRC = $(SRC_DIR)/pping.go
PPING     = $(BIN_DIR)/pping$(EXTENTION)

all: $(PPING)

$(BIN_DIR)/pping.exe:
	$(GO_BUILD) -o $(PPING) $(PPING_SRC)

$(BIN_DIR)/pping:
	$(GO_BUILD) -o $(PPING) $(PPING_SRC)
	sudo chown root $(PPING)
	sudo chmod 4555 $(PPING)

start: $(PPING)
	$(PPING) --host=192.168.0.5 --timeout=5000 --number=6 --interval=50
	$(PPING) --host=www.google.fr --timeout=5000 --number=6 --interval=50 --interactive=true
	$(PPING) --host=10.1.1.1 --timeout=5000 --number=6 --interval=50

start1: $(PPING)
	$(PPING) --host=192.168.0.6 --timeout=5000

clean:
	rm -f $(PPING)

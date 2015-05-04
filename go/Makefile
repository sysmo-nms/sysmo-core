GO_COMP ?= go

TARGET = pping$(GO_BIN_EXT)

$(TARGET): pping.go
	$(GO_COMP) build -o $(TARGET) pping.go

compile: $(TARGET)
clean:
	rm -f $(TARGET)

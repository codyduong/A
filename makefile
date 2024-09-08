PROJECT_NAME := ac

MAIN_DIR = .
TARGET_DIR = target/release
UNAME_S := $(shell uname -s | tr '[:upper:]' '[:lower:]')

.PHONY: all
all: build

ifeq ($(OS), Windows_NT)
    EXECUTABLE = $(PROJECT_NAME).exe
else
    EXECUTABLE = $(PROJECT_NAME)
endif


ifeq ($(OS), Windows_NT)
.PHONY: build
build:
	cargo build --release
	cp $(TARGET_DIR)/$(EXECUTABLE) $(MAIN_DIR)/$(EXECUTABLE)
else
.PHONY: build
build:
	cargo build --release
	cp $(TARGET_DIR)/$(EXECUTABLE) $(MAIN_DIR)/$(EXECUTABLE)
endif

.PHONY: test
test:
	cargo test --workspace

.PHONY: clean
clean:
	cargo clean

.PHONY: run
run: build
	cargo run --release

.PHONY: format
format:
	cargo fmt
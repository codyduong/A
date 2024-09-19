PROJECT_NAME := ac

MAIN_DIR = .
TARGET_DIR = target/release

.PHONY: all
all: build

.PHONY: build
build:
	cargo build --release
	cp $(TARGET_DIR)/$(PROJECT_NAME) $(MAIN_DIR)/$(PROJECT_NAME)

.PHONY: test
test:
	cargo test --workspace

.PHONY: clean
clean:
	cargo clean
	rm ./ac
	rm ./ac.exe
	rm ./target -r
	rm *.txt

.PHONY: run
run: build
	cargo run --release

.PHONY: format
format:
	cargo fmt
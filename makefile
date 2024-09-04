PROJECT_NAME := a

# Default target
.PHONY: all
all: build

# Build the project
.PHONY: build
build:
	cargo build --release

# Run tests
.PHONY: test
test:
	cargo test --workspace

# Clean the project
.PHONY: clean
clean:
	cargo clean

# Run the project
.PHONY: run
run: build
	cargo run --release

# Format the code
.PHONY: format
format:
	cargo fmt
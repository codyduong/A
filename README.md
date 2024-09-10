# A

*A* is the premier language written for EECS 665 - Compiler Construction @ KU - Fall 2024, taken with [Dr. Davidson](https://eecs.ku.edu/people/drew-davidson).

## Specification

The specification is archived here: [Language Specification](https://web.archive.org/web/20240908170812/https://compilers.cool/language/#lexical)

## Build

*ac* is written in [Rust](https://www.rust-lang.org/). Installation instructions are available here: https://www.rust-lang.org/tools/install

As a convenience a makefile is provided, as a simple wrapper around the equivalent cargo build commands.

## Run

```sh
make build
./ac input.a -t tokens.txt 2> errors.txt
```
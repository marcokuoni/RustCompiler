# Rust Compiler

Hand-written Rust compiler, plus a Nix flake dev shell.

## Dev shell

```bash
nix develop
```

## Build & test

```bash
cargo build
cargo test
cargo clippy -- -D warnings
cargo fmt -- --check
```

## Run demo

```bash
cargo run --bin compiler
```

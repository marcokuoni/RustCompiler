{
  description = "Nix dev shell for rust-lexer (stable Rust, clippy, rustfmt, rust-analyzer)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      rust-overlay,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs { inherit system overlays; };
        rust = pkgs.rust-bin.stable.latest;
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            rust.default # rustc, cargo, std
            rust.clippy # cargo clippy
            rust.rustfmt # rustfmt
            pkgs.rust-analyzer # LSP
            pkgs.cargo-nextest # fast tests (optional)
            pkgs.cargo-watch # dev convenience (optional)
            pkgs.pkg-config
          ];
          shellHook = ''
            echo "\nWelcome to the rust-lexer dev shell";
            echo "Commands: cargo build | cargo test | cargo clippy -- -D warnings | cargo fmt";
          '';
        };
      }
    );
}

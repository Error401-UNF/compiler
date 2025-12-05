{
  description = "A local development shell for Rust Project 1";

  inputs = {
    # Pin Nixpkgs to a specific commit/branch for reproducibility
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
  };

  outputs = { self, nixpkgs, ... }:
  let
    # Set the system architecture (adjust if not x86_64-linux)
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in
  {
    # Define a default development shell for this project
    devShells.${system}.default = pkgs.mkShell {
      name = "rust-project-1-dev";

      # The build tools required for this specific project
      nativeBuildInputs = with pkgs; [
        pkg-config
        gcc
        rustc              # Provides rustc
        rust-analyzer
        cargo
      ];

      # Libraries needed by the C-dependencies of this project
      # Example: If Project1 uses the 'sqlite' crate
      buildInputs = with pkgs; [
        sqlite # Specific library for Project 1
        musl
      ];

      shellHook = ''
        # AGGRESSIVE CLEANUP: Unset common environment variables that
        # often leak old, absolute library paths into the linker arguments (cc).
        unset RUSTFLAGS
        unset LDFLAGS
        unset CFLAGS
        
        export RUST_BACKTRACE=1
        echo "--------------------------------------------------------"
        echo "Rust Project 1 Environment Ready (Stable)."
        echo "Environment variables were cleaned to prevent linker path conflicts."
        echo "Use 'cargo build' or 'cargo check' to test your compiler."
        echo "--------------------------------------------------------"
      '';
    };
  };
}
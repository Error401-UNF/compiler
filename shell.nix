{ pkgs ? import <nixpkgs> {} }:

let
  # Use the latest available set of LLVM packages for current Nixpkgs.
  # This ensures compatibility with the inkwell crate's binding requirements.
  llvmPkgs = pkgs.llvmPackages_latest;

in
pkgs.mkShell {
  name = "rust-compiler-inkwell-chumsky-dev";

  # Native build tools required to compile Rust applications with native dependencies.
  nativeBuildInputs = with pkgs; [
    pkg-config
    gcc # Required for compiling C dependencies
    # Added native inputs for Rust toolchain
    rustup
    cargo
  ];

  # Libraries and toolchains included in the environment.
  buildInputs = with pkgs; [
    # 1. Rust Toolchain Components (Fixing 'undefined variable' error)
    # We include rustc/cargo directly, and rely on rustup being in nativeBuildInputs
    # to provide the complete development environment experience.
    rustc
    cargo

    # 2. LLVM Dependencies for Inkwell
    # 'libclang' is required by the 'clang-sys' crate, which Inkwell uses to find LLVM.
    llvmPkgs.libclang
    # The main LLVM distribution provides 'llvm-config' and the static/shared libraries.
    llvmPkgs.llvm
    
    # 3. FIX: Adding libffi to resolve 'cannot find -lffi' linker error.
    libffi

    # 4. FIX: Adding libxml2 to resolve 'cannot find -lxml2' linker error.
    libxml2
  ];

  # Directly set the TMPDIR attribute for mkShell
  TMPDIR = "/tmp";

  shellHook =
    let
      # Path to the directory containing libclang.so/dylib (used for LIBCLANG_PATH)
      libClangPath = "${llvmPkgs.libclang}/lib";
      # Path to the directory containing libLLVM.so/dylib (used for LD_LIBRARY_PATH)
      llvmCoreLibPath = "${llvmPkgs.llvm}/lib";
    in
    ''
      # Ensure cargo and rustup binaries are accessible, though Nix usually handles this.
      export PATH="$HOME/.cargo/bin:$PATH"
      export RUST_BACKTRACE=1
      
      # --- Inkwell / LLVM Configuration ---
      # This is crucial for the 'inkwell' crate to compile successfully.

      # 1. Tell 'clang-sys' (a dependency of inkwell) where to find libclang.so/dylib.
      export LIBCLANG_PATH="${libClangPath}"

      # 2. Ensure LLVM executables like 'llvm-config' are in the PATH.
      # Setting the prefix helps Rust build scripts find the configuration.
      export LLVM_SYS_PREFIX="${llvmPkgs.llvm}"
      
      # 3. CRUCIAL: Expose the LLVM shared libraries path to the linker (cc/ld).
      # This resolves 'linking with `cc` failed' errors when shared libs are needed.
      export LD_LIBRARY_PATH="${llvmCoreLibPath}:$LD_LIBRARY_PATH"

      echo "--------------------------------------------------------"
      echo "Rust Compiler Development Environment Ready (chumsky/inkwell)."
      echo "LLVM/libclang path configured: ${libClangPath}"
      echo "LLVM dynamic linker path configured: ${llvmCoreLibPath}"
      echo "Use 'cargo build' or 'cargo run'."
      echo "--------------------------------------------------------"
    '';
}

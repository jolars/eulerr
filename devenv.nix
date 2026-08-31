{
  pkgs,
  ...
}:

{
  packages = [
    pkgs.bashInteractive
    pkgs.checkbashisms
    pkgs.cargo-audit
    pkgs.cargo-deny
    pkgs.cargo-flamegraph
    pkgs.cargo-llvm-cov
    pkgs.cargo-msrv
    pkgs.go-task
    pkgs.llvmPackages.bintools
  ];

  languages = {
    rust = {
      enable = true;
      toolchainFile = ./src/rust/rust-toolchain.toml;
    };

    r = {
      enable = true;

      package = (
        pkgs.rWrapper.override {
          packages = with pkgs.rPackages; [
            covr
            devtools
            GenSA
            knitr
            lattice
            pBrackets
            polyclip
            polylabelr
            RConics
            rextendr
            rmarkdown
            spelling
            testthat
            urlchecker
            V8
            remotes
          ];
        }
      );
    };
  };

  # git-hooks = {
  #   hooks = {
  #     panache-format = {
  #       enable = true;
  #     };
  #   };
  # };
}

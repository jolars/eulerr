{
  pkgs,
  ...
}:

{
  packages = [
    pkgs.air-formatter
    pkgs.bashInteractive
    pkgs.checkbashisms
    pkgs.cargo-audit
    pkgs.cargo-deny
    pkgs.cargo-flamegraph
    pkgs.cargo-llvm-cov
    pkgs.cargo-msrv
    pkgs.go-task
    pkgs.jarl
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
            languageserver
            lattice
            pBrackets
            polyclip
            polylabelr
            RConics
            reprex
            rextendr
            rmarkdown
            spelling
            testthat
            urlchecker
            profvis
          ];
        }
      );

      lsp.enable = true;
    };
  };

  git-hooks = {
    hooks = {
      panache-format = {
        enable = true;
      };
    };
  };
}

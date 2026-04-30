{
  pkgs,
  ...
}:

{
  packages = [
    pkgs.air-formatter
    pkgs.bashInteractive
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
      channel = "stable";
      version = "1.94.1";
      targets = [ "wasm32-unknown-unknown" ];
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
          ];
        }
      );

      lsp.enable = true;
    };
  };
}

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
    pkgs.go-task
    pkgs.jarl
    pkgs.llvmPackages.bintools
  ];

  languages = {
    rust = {
      enable = true;
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
            Rcpp
            RcppArmadillo
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

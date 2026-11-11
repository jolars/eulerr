{
  pkgs,
  ...
}:

let
  # The archived CRAN release is marked broken in nixpkgs.
  nVennR = pkgs.rPackages.buildRPackage {
    pname = "nVennR";
    version = "0.2.4";
    src = pkgs.fetchFromGitHub {
      owner = "vqf";
      repo = "nVennR";
      rev = "fe4bc10e91335a1a73e959ff969e6a97bc189aec";
      hash = "sha256-4o6qNg2ACyuzaVgXadrThF2fXspYjjl+M42n6SdoPJY=";
    };
    propagatedBuildInputs = [ pkgs.rPackages.Rcpp ];
  };
in

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
            bench
            BioVenn
            covr
            devtools
            GenSA
            knitr
            lattice
            nVennR
            pBrackets
            polyclip
            polylabelr
            RConics
            rextendr
            remotes
            rmarkdown
            shiny
            spelling
            testthat
            urlchecker
            V8
            VennDiagram
            venneuler
            vennplot
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

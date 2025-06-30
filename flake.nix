{
  description = "A basic flake with a shell";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.systems.url = "github:nix-systems/default";
  inputs.flake-utils = {
    url = "github:numtide/flake-utils";
    inputs.systems.follows = "systems";
  };

  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          shellHook = ''
            mkdir -p "$(pwd)/_libs"
            export R_LIBS_USER="$(pwd)/_libs"
          '';
          packages =
            # let
            #   SLOPE = (
            #     pkgs.rPackages.buildRPackage {
            #       name = "SLOPE";
            #       src = ./.;
            #       propagatedBuildInputs = with pkgs.rPackages; [
            #         Matrix
            #         Rcpp
            #         RcppEigen
            #         covr
            #         knitr
            #         rmarkdown
            #         scales
            #         spelling
            #         testthat
            #         SparseM
            #         caret
            #         e1071
            #         bigmemory
            #         BH
            #       ];
            #     }
            #   );
            # in
            with pkgs; [
              bashInteractive
              autoconf
              go-task
              quartoMinimal
              llvmPackages.openmp
              (rWrapper.override {
                packages = with rPackages; [
                  devtools
                  GenSA
                  polyclip
                  polylabelr
                  Rcpp
                  RcppArmadillo
                  covr
                  knitr
                  lattice
                  pBrackets
                  RConics
                  rmarkdown
                  spelling
                  testthat
                ];
              })
            ];
        };
      }
    );
}

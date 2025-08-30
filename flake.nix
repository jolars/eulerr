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
          packages =
            let
              eulerr = (
                pkgs.rPackages.buildRPackage {
                  name = "eulerr";
                  src = ./.;
                  propagatedBuildInputs = with pkgs.rPackages; [
                    GenSA
                    polyclip
                    polylabelr
                    Rcpp
                    RcppArmadillo
                    knitr
                    rmarkdown
                    testthat
                    lattice
                    pBrackets
                    RConics
                    spelling
                    covr
                  ];
                }
              );
            in
            with pkgs;
            [
              bashInteractive
              autoconf
              go-task
              quartoMinimal
              llvmPackages.openmp
              (rWrapper.override {
                packages = with rPackages; [
                  devtools
                  eulerr
                ];
              })
            ];
        };
      }
    );
}

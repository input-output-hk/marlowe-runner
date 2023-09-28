# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#38-nixformattersnix

{ iogxRepoRoot, repoRoot, inputs, inputs', pkgs, system, lib, ... }:

{
  # cabal-fmt.enable = false;
  # cabal-fmt.extraOptions = "";

  # stylish-haskell.enable = false;
  # stylish-haskell.extraOptions = "";

  shellcheck.enable = true;
  shellcheck.extraOptions = "";

  # prettier.enable = false;
  # prettier.extraOptions = "";

  # editorconfig-checker.enable = false;
  # editorconfig-checker.extraOptions = "";

  nixpkgs-fmt.enable = true;
  nixpkgs-fmt.extraOptions = "";

  # png-optimization.enable = false;
  # png-optimization.extraOptions = "";

  # fourmolu.enable = false;
  # fourmolu.extraOptions = "";

  purs-tidy.enable = true;
  purs-tidy.extraOptions = "";

  # hlint.enable = false;
  # hlint.extraOptions = "";
}

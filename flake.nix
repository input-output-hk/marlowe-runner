# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#31-flakenix

{
  description = "Change the description field in your flake.nix";


  inputs = {
    iogx.url = "github:input-output-hk/iogx";
    easyPSSrc = {
      flake = false;
      url = "github:justinwoo/easy-purescript-nix";
    };
    easyPSSrcPaluh = {
      flake = false;
      url = "github:paluh/easy-purescript-nix/16c99dd8487604f5bc6b2aa86d33cacf8dacef8c";
    };
  };


  outputs = inputs: inputs.iogx.lib.mkFlake {
    inherit inputs;
    repoRoot = ./.;
    # systems = ["x86_64-linux" "x86_64-darwin"];
  };


  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };
}

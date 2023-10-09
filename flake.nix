{
  description = "Marlowe Runner";

  inputs = {
    iogx.url = "github:input-output-hk/iogx?ref=v4";

    spago2nix = {
      url = "github:justinwoo/spago2nix";
      inputs.easy-purescript-nix.follows = "easyPSSrc";
    };

    easyPSSrc = {
      flake = false;
      url = "github:justinwoo/easy-purescript-nix/5dcea83eecb56241ed72e3631d47e87bb11e45b9";
    };
  };

  outputs = inputs: inputs.iogx.lib.mkFlake {
    inherit inputs;
    repoRoot = ./.;
    systems = [ "x86_64-linux" "x86_64-darwin" ];
    outputs = import ./nix/outputs.nix;
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


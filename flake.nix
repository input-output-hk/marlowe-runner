{
  description = "Marlowe Runner";

  inputs = {
    easyPSSrc.url = "github:justinwoo/easy-purescript-nix";
    spago2nix = {
      url = "github:justinwoo/spago2nix";
      inputs.easy-purescript-nix.follows = "easyPSSrc";
    };

    falke-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs @ { self, nixpkgs, flake-utils, ... }: flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      pkgs = import nixpkgs { inherit system; };
      lib = pkgs.lib;
      repoRoot.nix = {
          shell = import ./nix/shell.nix { inherit repoRoot inputs pkgs lib system; };
          marlowe-runner = import ./nix/marlowe-runner.nix { inherit repoRoot inputs pkgs lib system; };
          purescript = import ./nix/purescript.nix { inherit inputs pkgs system; };
          playwright = import ./nix/playwright.nix { inherit pkgs; };
      };
      # repoRoot = { inherit (self) nix; };
    in {
      packages = {
        marlowe-runner = repoRoot.nix.marlowe-runner;
        default = self.packages.${system}.marlowe-runner;  # Convenience: nix build .#
      };

      devShells = {
        default = pkgs.mkShell (repoRoot.nix.shell // { inherit (repoRoot.nix.shell) env; });
      };

    }
  ) // { nixosModules.default = import ./nix/nixos.nix self; };

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


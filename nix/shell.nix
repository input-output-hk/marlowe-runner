{ repoRoot, inputs, pkgs, lib, system }:

let

  purescript = repoRoot.nix.purescript;

in

lib.iogx.mkShell {

  name = "marlowe-runner";

  packages = [
    # Please update spago and purescript in `package.json` `scripts` section
    purescript.purs-0_15_10
    purescript.purescript-language-server
    purescript.purs-backend-es
    purescript.pscid
    purescript.pulp
    purescript.spago
    purescript.spago2nix

    pkgs.podman
    pkgs.jq
    pkgs.docker
    pkgs.nodejs-18_x
    pkgs.nodejs-18_x.pkgs.bower
    pkgs.nodejs-18_x.pkgs.jshint
    pkgs.nodejs-18_x.pkgs.yarn
    pkgs.nodejs-18_x.pkgs.webpack-cli
    pkgs.dhall
    pkgs.pkg-config
    pkgs.python38
  ];

  shellHook = ''
    # NODE_OPTIONS="--experimental-fetch --trace-warnings"
    # export PATH="$PATH:./node_modules/.bin/:./bin"
  '';

  scripts.run-spago2nix = {
    group = "marlowe-runner";
    description = "Run spago2nix to generate ./spago-packages.nix";
    exec = ''
      cd "$(git rev-parse --show-toplevel)"
      ${purescript.spago2nix}/bin/spago2nix generate
    '';
  };

  tools = {
    purs-tidy = purescript.purs-tidy;
  };

  preCommit = {
    shellcheck.enable = true;
    nixpkgs-fmt.enable = true;
    purs-tidy.enable = false;
  };
}

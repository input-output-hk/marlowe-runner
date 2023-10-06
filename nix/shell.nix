{ repoRoot, inputs, pkgs, lib, system }:

let

  purescript = repoRoot.nix.purescript;

in
lib.iogx.mkShell {

  name = "marlowe-runner";
  env = {
    PLAYWRIGHT_BROWSERS_PATH =
      lib.optionalString (system == "x86_64-linux")
        "${repoRoot.nix.playwright}";

    PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = "true";
  };

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
    pkgs.nodejs-16_x
    pkgs.nodejs-16_x.pkgs.bower
    pkgs.nodejs-16_x.pkgs.jshint
    pkgs.nodejs-16_x.pkgs.yarn
    pkgs.nodejs-16_x.pkgs.webpack-cli
    pkgs.dhall
    pkgs.pkg-config
    pkgs.python38
  ];

  scripts.gen-spago-packages-nix = {
    group = "marlowe-runner";
    description = "Run spago2nix to generate ./spago-packages.nix";
    exec = ''
      cd "$(git rev-parse --show-toplevel)"
      ${purescript.spago2nix}/bin/spago2nix generate
    '';
  };

  shellHook = ''
    NODE_OPTIONS="--experimental-fetch --trace-warnings"
  '';

  tools = {
    purs-tidy = purescript.purs-tidy;
  };

  preCommit = {
    shellcheck.enable = true;
    nixpkgs-fmt.enable = true;
    purs-tidy.enable = true;
  };
}

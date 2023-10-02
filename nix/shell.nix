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
    pkgs.dhall
    pkgs.pkg-config
    pkgs.python38
  ];

  shellHook = ''
    npm install
    NODE_OPTIONS="--experimental-fetch --trace-warnings"
    export PATH="$PATH:./node_modules/.bin/:./bin"
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
    # haskellCompilerVersion = "ghc8107";
    # cabal-fmt = null;
    # cabal-install = null;
    # haskell-language-server = null;
    # haskell-language-server-wrapper = null;
    # fourmolu = null;
    # hlint = null;
    # stylish-haskell = null;
    # ghcid = null;
    # shellcheck = null;
    # prettier = null;
    # editorconfig-checker = null;
    # nixpkgs-fmt = null;
    # optipng = null;
  };

  preCommit = {
    shellcheck.enable = true;
    nixpkgs-fmt.enable = true;
    # cabal-fmt.enable = false;
    # cabal-fmt.extraOptions = "";
    # stylish-haskell.enable = false;
    # stylish-haskell.extraOptions = "";
    # fourmolu.enable = false;
    # fourmolu.extraOptions = "";
    # hlint.enable = false;
    # hlint.extraOptions = "";
    # shellcheck.extraOptions = "";
    # prettier.enable = false;
    # prettier.extraOptions = "";
    # editorconfig-checker.enable = false;
    # editorconfig-checker.extraOptions = "";
    # nixpkgs-fmt.enable = false;
    # nixpkgs-fmt.extraOptions = "";
    # optipng.enable = false;
    # optipng.extraOptions = "";
    # purs-tidy.enable = false;
    # purs-tidy.extraOptions = "";
  };

  # prompt = null;

  # welcomeMessage = null;

  # env = { };

  # scripts = {};
}

# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#34-nixshellnix

{ iogxRepoRoot, repoRoot, inputs, inputs', pkgs, system, lib, project ? null, ... }:

let

  easyPS = pkgs.callPackage inputs.easyPSSrc { inherit pkgs; };
  easyPSPaluh = pkgs.callPackage inputs.easyPSSrcPaluh { inherit pkgs; };
  nodejs-16 = pkgs.writeShellScriptBin "nodejs-16" ''
    ${ pkgs.nodejs-16_x.out}/bin/node $@
  '';

in

  {
    # name = "nix-shell";
    # prompt = "$ ";
    welcomeMessage = "Marlowe Runner";
    packages = [
      # Please update spago and purescript in `package.json` `scripts` section
      easyPSPaluh."purs-0_15_10_0"
      easyPSPaluh."purs-tidy"
      easyPS.purescript-language-server
      easyPS.pscid
    # easyPS.purs-tidy
      easyPS.pulp
      easyPS.spago
      pkgs.jq
      pkgs.docker
      pkgs.nodePackages.bower
      pkgs.nodePackages.jshint
      pkgs.nodePackages.nodemon
      pkgs.nodePackages.yarn
      pkgs.nodePackages.webpack
      pkgs.nodePackages.webpack-cli
      pkgs.nodePackages.webpack-dev-server
      pkgs.dhall
      pkgs.nodejs-18_x
      nodejs-16
      pkgs.pkg-config
      pkgs.postgresql
      pkgs.python27
      pkgs.python38
      pkgs.unzip
      pkgs.nixpacks
    ];
    # scripts = { };
    # env = { };
    enterShell = ''
      npm install
      NODE_OPTIONS=--experimental-fetch --trace-warnings
      export PATH=$PATH:./node_modules/.bin/:./bin
    '';
  }

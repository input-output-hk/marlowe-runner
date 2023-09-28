# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#34-nixshellnix
{ iogxRepoRoot, repoRoot, inputs, inputs', pkgs, system, lib, project ? null, ... }:

let
  easyPS = pkgs.callPackage inputs.easyPSSrc { inherit pkgs; };
in
{
  # name = "nix-shell";
  # prompt = "$ ";
  welcomeMessage = "Marlowe Runner";
  packages = [
    pkgs.podman
    # Please update spago and purescript in `package.json` `scripts` section
    easyPS."purs-0_15_10"
    easyPS."purs-tidy"
    easyPS.purescript-language-server
    easyPS.pscid
    easyPS.pulp
    easyPS.spago
    pkgs.jq
    pkgs.docker
    pkgs.nodePackages.bower
    pkgs.nodePackages.jshint
    pkgs.nodePackages.yarn
    pkgs.dhall
    pkgs.nodejs-18_x
    pkgs.pkg-config
    pkgs.python38
  ];
  # scripts = { };
  # env = { };
  enterShell = ''
    npm install
    NODE_OPTIONS="--experimental-fetch --trace-warnings"
    export PATH="$PATH:./node_modules/.bin/:./bin"
  '';
}

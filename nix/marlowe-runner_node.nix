{ repoRoot, inputs, pkgs, lib, system }:

let

  npmlock2nix = import inputs.npmlock2nix { inherit pkgs; };

  spagoPkgs = import (inputs.self + "/spago-packages.nix") { inherit pkgs; };

in

npmlock2nix.v2.node_modules {

  nodejs = pkgs.nodejs-18_x;

  src = lib.sourceByRegex ../. [
    "^prototype.*"
    "^public.*"
    "^src.*"
    "^test.*"
    "^.tidyrc.json$"
    "^jsconfig.json$"
    "^package-lock.json$"
    "^package.json$"
    "^packages.dhall$"
    "^prod.dhall$"
    "^spago-packages.nix$"
    "^spago.dhall$"
    "^tsconfig.json$"
    "^webpack.js$"
  ];
}
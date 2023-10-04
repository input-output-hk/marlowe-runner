{ repoRoot, inputs, pkgs, lib, system }:

let

  npmlock2nix = import inputs.npmlock2nix { inherit pkgs; };

  spagoPkgs = import (inputs.self + "/spago-packages.nix") { inherit pkgs; };

in

npmlock2nix.v2.build {

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

  buildInputs = [
    pkgs.which
    spagoPkgs.installSpagoStyle
    spagoPkgs.buildSpagoStyle
    repoRoot.nix.purescript.purs
    repoRoot.nix.purescript.spago2nix
  ];

  buildCommands = [
    ''
      mkdir -p dist
      cp -r $src/* dist 
      cd dist 

      mv prod.dhall spago.dhall
      install-spago-style
      build-spago-style "./src/**/*.purs"
      export MARLOWE_WEB_SERVER_URL=http://localhost:3780
      webpack-cli --mode=production -c webpack.js
    ''
  ];

  installPhase = "cp -r dist $out";
}

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
    repoRoot.nix.purescript.purs-0_15_10
    repoRoot.nix.purescript.spago2nix
  ];

  buildCommands = [
    ''
      mkdir -p dist
      cp -r $src/* dist 
      chmod -R u+w dist       
      cd dist 

      mv prod.dhall spago.dhall
      install-spago-style
      build-spago-style "./src/**/*.purs"
      echo '{ "marloweWebServerUrl": "https://marlowe-runtime-preprod-web.scdev.aws.iohkdev.io", "develMode": false }' > public/config.json
      webpack-cli --mode=production -c webpack.js
      cd ..
    ''
  ];

  installPhase = "cp -r dist/public $out";
}

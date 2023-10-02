{ repoRoot, inputs, pkgs, ... }:

let

  hackedPkgs = pkgs // {
    nodejs-16_x = pkgs.nodejs-18_x;
  };

  npmlock2nix = import inputs.npmlock2nix { pkgs = hackedPkgs; };

  spagoPkgs = import (inputs.self + "/spago-packages.nix") { inherit pkgs; };

in

npmlock2nix.v2.build {

  src = inputs.self;

  buildInputs = [
    # pkgs.nodejs-18_x
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
      ${pkgs.nodejs-18_x.pkgs.webpack-cli}/bin/webpack-cli --mode=production -c webpack.js
      ls -lah
    ''
  ];

  installPhase = "cp -r dist $out";
}

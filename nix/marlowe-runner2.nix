{ repoRoot, inputs, pkgs, ... }:

let
  npmlock2nix = import inputs.npmlock2nix { inherit pkgs; };

  spagoPkgs = import (inputs.self + "/spago-packages.nix") { inherit pkgs; };
in

npmlock2nix.v1.build {

  src = inputs.self;

  installPhase = "cp -r dist $out";

  node_modules_attrs = {
    # githubSourceHashMap = {
    #   shmish111.nearley-webpack-loader."939360f9d1bafa9019b6ff8739495c6c9101c4a1" = "1brx669dgsryakf7my00m25xdv7a02snbwzhzgc9ylmys4p8c10x";
    #   ankitrohatgi.tarballjs."64ea5eb78f7fc018a223207e67f4f863fcc5d3c5" = "04r9yap0ka4y3yirg0g7xb63mq7jzc2qbgswbixxj8s60k6zdqsm";
    # };
    # buildInputs = [ pkgs.python ];
  };

  buildInputs = [
    pkgs.nodejs-18_x
    spagoPkgs.installSpagoStyle
    spagoPkgs.buildSpagoStyle
    repoRoot.nix.purescript.purs
    repoRoot.nix.purescript.spago2nix
  ];

  unpackPhase = ''
    mkdir -p marlowe-runner
    cp -r $src/* marlowe-runner
    cd marlowe-runner
    install-spago-style
    cd ..
  '';

  buildCommands = [
    ''
      cd marlowe-runner
      build-spago-style \
        "./src/**/*.purs" \
        "./generated/**/*.purs"
      rm -rf ./dist
      npm run build:webpack:prod
    ''
  ];
}

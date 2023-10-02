{ repoRoot, inputs, pkgs, ... }:

let
  npmlock2nix = import inputs.npmlock2nix { inherit pkgs; };

  spagoPkgs = import (inputs.self + "/spago-packages.nix") { inherit pkgs; };

  # spago-packages = inputs.spago2nix.packages.spago2nix_nativeBuildInputs {
  #   spago-dhall = "prod.dhall";
  #   srcs-dhall = [ ./prod.dhall ./packages.dhall ];
  # };

in

pkgs.stdenv.mkDerivation {
  name = "marlowe-runner";

  buildInputs = [
    repoRoot.nix.purescript.spago
    repoRoot.nix.purescript.purs
    pkgs.which
  ];

  src = ./..;

  # SYSTEM_CERTIFICATE_PATH = "${pkgs.cacert}/etc/ssl/certs";
  # SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
  # NIX_SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
  # GIT_SSL_CAINFO = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";

  installPhase = ''
    # /nix/store/0an0dyxzgwmsm6lyld7b0hiq6plkpbq0-spago-0.21.0/bin/spago
    # /nix/store/09h72wxap24qkm1icj602abr1layxjm7-purescript-v0.15.10/bin/purs
    export HOME=$TMP
    mkdir -p $out
    cd $src
    echo aaaaaaa
    which spago 
    which purs 
    purs compile ./Main.purs
    # spago -x spago.dhall  build 

    # --purs-args '--codegen corefn,js'  -V               
    echo aaaaaaa
    #purs-backend-es bundle-app --minify --platform=node --main Test.Main --to index.mjs  
    #node index.mjs 
    #spago build -V --global-cache skip
    #spago build -- --config prod.dhall --global-cache skip
    ls -lah
    mv outputs $out/
  '';
}

# }

# npmlock2nix.v1.build {

#   src = inputs.self;

#   installPhase = "cp -r dist $out";

#   node_modules_attrs = {
#     # githubSourceHashMap = {
#     #   shmish111.nearley-webpack-loader."939360f9d1bafa9019b6ff8739495c6c9101c4a1" = "1brx669dgsryakf7my00m25xdv7a02snbwzhzgc9ylmys4p8c10x";
#     #   ankitrohatgi.tarballjs."64ea5eb78f7fc018a223207e67f4f863fcc5d3c5" = "04r9yap0ka4y3yirg0g7xb63mq7jzc2qbgswbixxj8s60k6zdqsm";
#     # };
#     # buildInputs = [ pkgs.python ];
#   };

#   buildInputs = [
#     pkgs.nodejs-18_x
#     spagoPkgs.installSpagoStyle
#     spagoPkgs.buildSpagoStyle
#     repoRoot.nix.purescript.purs
#     repoRoot.nix.purescript.spago2nix
#   ];

#   unpackPhase = ''
#     mkdir -p marlowe-runner
#     cp -r $src/* marlowe-runner
#     cd marlowe-runner
#     install-spago-style
#     cd ..
#   '';

#   buildCommands = [
#     ''
#       cd marlowe-runner
#       build-spago-style \
#         "./src/**/*.purs" \
#         "./generated/**/*.purs"
#       rm -rf ./dist
#       npm run build:webpack:prod
#     ''
#   ];
# }

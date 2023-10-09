{ repoRoot, inputs, pkgs, lib, system }:

let

  spago-pkgs = import ./gen/spago-packages.nix { inherit pkgs; };

in

pkgs.buildNpmPackage {

  pname = "marlowe-runner";

  version = "0.1.0";

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

  npmDepsHash = import ./gen/npm-deps-hash.nix;

  nativeBuildInputs = [
    spago-pkgs.installSpagoStyle
    spago-pkgs.buildSpagoStyle
    repoRoot.nix.purescript.purs-0_15_10
    repoRoot.nix.purescript.spago2nix
    pkgs.nodejs_18.pkgs.webpack-cli
  ];

  buildPhase = ''
    mkdir -p $out

    install-spago-style
    build-spago-style "./src/**/*.purs"

    # This will create the public/*bundle.js* and public/*.module.wasm files.   
    webpack-cli --mode=production -c webpack.js --progress=profile

    cp -r public/* $out
  '';

  dontNpmBuild = true;
  dontNpmInstall = true;
}

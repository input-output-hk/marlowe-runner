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

  # buildNpmPackage is able to make a pure nix build by using npmDepsHash.
  # That is the hash of package-lock.json.
  # Its value is generated using the prefetch-npm-deps command (see shell.nix).
  # We set dontNpmBuild and dontNpmInstall to true to significantly speed up the 
  # build: this works because we have a custom buildPhase that invokes webpack-cli
  # explicitely.
  npmDepsHash = import ./gen/npm-deps-hash.nix;

  nativeBuildInputs = [
    spago-pkgs.installSpagoStyle
    spago-pkgs.buildSpagoStyle
    repoRoot.nix.purescript.purs-0_15_10
    repoRoot.nix.purescript.spago2nix
    pkgs.nodejs_18
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

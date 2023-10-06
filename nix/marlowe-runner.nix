{ repoRoot, inputs, pkgs, lib, system }:

let

  npmlock2nix = import inputs.npmlock2nix { inherit pkgs; };

  spago-pkgs = import ./gen/spago-packages.nix { inherit pkgs; };

  node-deps =
    let
      node-default = import ./gen/node-default.nix {
        inherit pkgs system;
        nodejs = pkgs.nodejs-18_x;
      };
    in
    node-default.nodeDependencies;

in

pkgs.stdenv.mkDerivation {

  name = "marlowe-runner";

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

  buildInputs = [
    spago-pkgs.installSpagoStyle
    spago-pkgs.buildSpagoStyle
    repoRoot.nix.purescript.purs-0_15_10
    repoRoot.nix.purescript.spago2nix
  ];

  buildPhase = ''
    # Symlink the generated node deps to the current directory for building
    ln -sf ${node-deps}/lib/node_modules ./node_modules
    export PATH="${node-deps}/bin:$PATH"

    # We want to use prod.dhall, but `install-spago-style` below uses 
    # spago.dhall by default and there doesn't seem to be a way to override it.
    mv prod.dhall spago.dhall"
      
    install-spago-style
    build-spago-style "./src/**/*.purs"

    # This will create the public/*bundle.js* and public/*.module.wasm files.   
    webpack-cli --mode=production -c webpack.js --progress=profile
  '';

  installPhase = ''
    cp -r public $out

    # runHook preInstall
    # mkdir -p $out/bin
    # cp package.json $out/package.json
    # cp -r dist $out/dist
    # ln -sf ${node-deps}/lib/node_modules $out/node_modules
    # # copy entry point, in this case our index.ts has the node shebang
    # # nix will patch the shebang to be the node version specified in buildInputs
    # # you could also copy in a script that is basically `npm run start`
    # cp dist/index.js $out/bin/example-ts-nix
    # chmod a+x $out/bin/example-ts-nix
    # runHook postInstall
  '';
}

# npmlock2nix.v1.build {

#   nodejs = pkgs.nodejs-18_x;

#   src = lib.sourceByRegex ../. [
#     "^prototype.*"
#     "^public.*"
#     "^src.*"
#     "^test.*"
#     "^.tidyrc.json$"
#     "^jsconfig.json$"
#     "^package-lock.json$"
#     "^package.json$"
#     "^packages.dhall$"
#     "^prod.dhall$"
#     "^spago-packages.nix$"
#     "^spago.dhall$"
#     "^tsconfig.json$"
#     "^webpack.js$"
#   ];

#   buildInputs = [
#     spagoPkgs.installSpagoStyle
#     spagoPkgs.buildSpagoStyle
#     repoRoot.nix.purescript.purs-0_15_10
#     repoRoot.nix.purescript.spago2nix
#   ];

#   buildCommands = [
#     "mkdir -p dist"
#     "cp -r $src/* dist"

#     # webpack-cli will want to write file to dist/public, so we need perissions.     
#     "chmod -R u+w dist"

#     "cd dist"

#     # We want to use prod.dhall, but `install-spago-style` below uses 
#     # spago.dhall by default and there doesn't seem to be a way to override it.
#     "mv prod.dhall spago.dhall"
#     "echo AM I FAILING"
#     "install-spago-style"
#     "echo OR sME"
#     "build-spago-style \"./src/**/*.purs\""
#     "echo OR NPM"

#     # These appear to the fix the following error when building on Hydra:
#     #   npm ERR! request to https://registry.npmjs.org/express failed, reason: getaddrinfo 
#     "npm config rm proxy"
#     "npm config rm https-proxy --tried removing npm proxy"

#     # This will create the public/*bundle.js* and public/*.module.wasm files.   
#     "webpack-cli --mode=production -c webpack.js --progress=profile"
#   ];

#   # The output of the nix build is the contents of the ./public folder 
#   installPhase = "cp -r public $out";
# }

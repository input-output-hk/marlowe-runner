{ iogxRepoRoot, repoRoot, inputs, inputs', pkgs, system, lib }:


# Note that one cannot use `npmlock2nix` for this package because is suffers from the sandboxing problem
# described in <https://johns.codes/blog/building-typescript-node-apps-with-nix#the-standard-environment>.
#
# FIXME: However, `node2nix` also fails due to `leveldown` calling `node-gyp-build` without the shebangs
# replaced for nix. This also seems to involve npm packages doing native builds.


# This file has been generated by node2nix 1.11.1. Do not edit!
let
  nodejs = pkgs.nodejs-18_x;
  nodeEnv = import ./node-env.nix {
    inherit (pkgs) stdenv lib python2 runCommand writeTextFile writeShellScript;
    inherit pkgs nodejs;
    libtool = if pkgs.stdenv.isDarwin then pkgs.darwin.cctools else null;
  };
  nodePackages =
    import ./node-packages.nix {
      inherit (pkgs) fetchurl nix-gitignore stdenv lib fetchgit;
      inherit nodeEnv;
    };
  nodeDependencies = nodePackages.nodeDependencies;
in
pkgs.stdenv.mkDerivation {
  name = "marlowe-runner";
  src = ./../..;
  buildInputs = [
    nodejs
  ];
  buildPhase = ''
    ln -s ${nodeDependencies}/lib/node_modules ./node_modules
    export PATH="${nodeDependencies}/bin:$PATH"
    # Build the distribution bundle in "dist"
    webpack
    cp -r dist $out/
  '';
}

# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#35-nixper-system-outputsnix

{ iogxRepoRoot, repoRoot, inputs, inputs', pkgs, system, lib, projects ? null, ... }:

let

  marlowe-runner = repoRoot.nix.marlowe-runner.default;

in

{
  packages = {
    marlowe-runner = marlowe-runner;
  };
  # checks = { };
  # apps = { };
  operables = repoRoot.nix.marlowe-runner.deploy.operable;
  oci-images =
    lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux
      repoRoot.nix.marlowe-runner.deploy.oci-image;
  # nomadTasks = { };
  # foobar = { };
}

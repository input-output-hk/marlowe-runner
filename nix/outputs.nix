{ repoRoot, inputs, pkgs, lib, system }:

[
  {
    devShells.default = repoRoot.nix.shell;

    packages.marlowe-runner = repoRoot.nix.marlowe-runner;

    operables = repoRoot.nix.marlowe-runner.deploy.operables;

    oci-images = repoRoot.nix.marlowe-runner.deploy.oci-image;

    hydraJobs.required = lib.iogx.mkHydraRequiredJob { };

    inherit repoRoot;
  }
]

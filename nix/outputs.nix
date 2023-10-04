{ repoRoot, inputs, pkgs, lib, system }:

[
  {
    devShells.default = repoRoot.nix.shell;

    packages.marlowe-runner = repoRoot.nix.marlowe-runner;

    operables = repoRoot.nix.deploy.operables;

    oci-images = repoRoot.nix.deploy.oci-images;

    hydraJobs.required = lib.iogx.mkHydraRequiredJob { };

    inherit repoRoot;
  }
]

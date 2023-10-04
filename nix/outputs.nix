{ repoRoot, inputs, pkgs, lib, system }:

[
  {
    devShells.default = repoRoot.nix.shell;

    packages.marlowe-runner = repoRoot.nix.marlowe-runner;

    operables = repoRoot.nix.deploy.operables;
    oci-images = repoRoot.nix.deploy.oci-images;

    hydraJobs.packages.marlowe-runner = repoRoot.nix.marlowe-runner;
    hydraJobs.devShells.default = repoRoot.nix.shell;
    hydraJobs.required = lib.iogx.mkHydraRequiredJob { };

    # For debugging purposes. 
    inherit repoRoot;
  }
]

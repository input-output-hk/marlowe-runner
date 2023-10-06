{ repoRoot, inputs, pkgs, lib, system }:

[
  {
    devShells.default = repoRoot.nix.shell;

    packages.marlowe-runner = repoRoot.nix.marlowe-runner;
    packages.marlowe-runner_node = repoRoot.nix.marlowe-runner_node;

    hydraJobs.packages.marlowe-runner = repoRoot.nix.marlowe-runner;
    hydraJobs.packages.marlowe-runner_node = repoRoot.nix.marlowe-runner_node;
    hydraJobs.devShells.default = repoRoot.nix.shell;
    hydraJobs.required = lib.iogx.mkHydraRequiredJob { };

    # For debugging purposes. 
    inherit repoRoot;
  }
]

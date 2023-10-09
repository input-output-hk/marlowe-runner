{ repoRoot, inputs, pkgs, lib, system }:

[
  {
    devShells.default = repoRoot.nix.shell;

    packages.marlowe-runner = repoRoot.nix.marlowe-runner;

    hydraJobs.packages.marlowe-runner = repoRoot.nix.marlowe-runner;
    hydraJobs.devShells.default = repoRoot.nix.shell;
    hydraJobs.required = lib.iogx.mkHydraRequiredJob { };
  }
]

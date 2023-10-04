{ repoRoot, inputs, pkgs, lib, system }:

{
  marlowe-runner = inputs.std.lib.ops.mkOperable {
    package = repoRoot.nix.marlowe-runner;
    runtimeInputs = [ pkgs.darkhttpd ];
    runtimeScript = ''
      exec darkhttpd "''${CONFIG_HTML_ROOT:-${repoRoot.nix.marlowe-runner}}" --addr 0.0.0.0 --port 8080
    '';
  };
}

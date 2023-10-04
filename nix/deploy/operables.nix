{ repoRoot, inputs, pkgs, lib, system }:

{
  marlowe-runner = inputs.std.lib.ops.mkOperable {
    package = repoRoot.nix.marlowe-runner;
    runtimeInputs = [ pkgs.darkhttpd ];
    runtimeScript = ''
      exec darkhttpd "''${CONFIG_HTML_ROOT:-${repoRoot.nix.marlowe-runner}}" --port 8080 --mimetypes ${pkgs.mailcap}/etc/mime.types
    '';
  };
}

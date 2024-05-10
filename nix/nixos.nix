self: { lib, config, pkgs, ... }:
let
  inherit (lib) mkOption types mapAttrs';

  inherit (pkgs) writeTextDir symlinkJoin;

  runnerOptions = { name, ... }: {
    options = {
      domain = mkOption {
        type = types.str;
        default = name;
        description = "The domain to host marlowe-runner";
      };

      # TODO local or remote
      runtime-instance = mkOption {
        type = types.str;
        description = "The name of the runtime instance to connect to";
      };

      flake = mkOption {
        type = types.attrs;
        default = self;
        description = "A Nix Flake containing the runner application";
      };
    };
  };

  mkRoot = name: { runtime-instance, flake, ... }:
    let
      configJson = writeTextDir "config.json" (builtins.toJSON {
        marloweWebServerUrl = "//${config.marlowe.runtimes.${runtime-instance}.domain}";
        develMode = false;
      });
    in
    symlinkJoin {
      name = "marlowe-runner-${name}-root";
      paths = [
        flake.packages.${pkgs.system}.marlowe-runner
        configJson
      ];
    };
in
{
  options = {
    marlowe.runners = mkOption {
      type = types.attrsOf (types.submodule runnerOptions);
      default = { };
      description = "Marlowe Runner instances to run";
    };
  };
  config = {
    http-services.static-sites = mapAttrs'
      (name: runner: {
        name = "marlowe-runner-${name}";
        value = {
          inherit (runner) domain;
          root = mkRoot name runner;
          index-fallback = true;
        };
      })
      config.marlowe.runners;
  };
}

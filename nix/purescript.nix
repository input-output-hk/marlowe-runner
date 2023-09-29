{ inputs, pkgs, system, ... }:

pkgs.callPackage inputs.easyPSSrc { inherit pkgs; }

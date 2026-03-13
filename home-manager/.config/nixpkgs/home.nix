args@{ config, lib, pkgs, profile ? "full", ... }:
let
  currentSystem = pkgs.system;
in
import ./base.nix (args // { inherit profile currentSystem; })

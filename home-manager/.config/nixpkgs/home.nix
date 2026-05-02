args@{ config, lib, pkgs, profile ? "full", ... }:
let
  currentSystem = pkgs.stdenv.hostPlatform.system;
in
import ./base.nix (args // { inherit profile currentSystem; })

args@{ config, lib, pkgs, minimal ? false, ... }:
let
  currentSystem = pkgs.system;
in
import ./base.nix (args // { inherit minimal currentSystem; })

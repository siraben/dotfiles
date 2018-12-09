{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.mine.workstation.recap;

  recap = import (pkgs.fetchFromGitHub {
    owner = "dustinlacewell";
    repo = "recap";
    rev = "ac4e74dd0e24236636947e408de81930f7b8d3e2";
    sha256 = "023cj5i71x7z418r3s53gm1mhnd2df5ijnvnpgdfhln6y3m72xp5";
  });

  # recap = import /home/ldlework/src/recap;

in {
  options.mine.workstation.recap.enable = mkEnableOption "recap";

  config = mkIf cfg.enable {
    environment.systemPackages = [ recap ];
  };
}


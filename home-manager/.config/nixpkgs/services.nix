{ config, lib, pkgs, ... }:

{
  config = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
    services.ssh-agent.enable = true;
    # Let Home Manager preserve forwarded agents in its early shell hooks.
    # Minimal shells may lack XDG_RUNTIME_DIR, even on NixOS.
    sshAuthSock.initialization.bash = lib.mkForce ''
      export SSH_AUTH_SOCK="''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/${config.services.ssh-agent.socket}"
    '';
  };
}

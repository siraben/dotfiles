{ config, lib, currentSystem, profile, inputs, ... }:

let
  inherit (lib.systems.elaborate { system = currentSystem; }) isLinux isDarwin;
  unfreePackages = [
    "discord"
    "slack"
    "spotify"
    "spotify-unwrapped"
    "zoom"
    "aspell-dict-en-science"
    "claude-code"
  ];
  pkgsOptions = {
    overlays = [
      (import ./overlay.nix { inherit inputs; })
      inputs.siraben-overlay.overlays.default
    ];
    config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) unfreePackages;
  };
  pkgs = import inputs.nixpkgs {
    system = currentSystem;
    inherit (pkgsOptions) overlays config;
  };
  forgejoMcpPort = 8214;
  forgejoMcpTokenPath = "/home/siraben/psi-coding-agent/forgejo_token.txt";
  forgejoMcpStart = pkgs.writeShellScript "forgejo-mcp-start" ''
    set -euo pipefail

    credential="$CREDENTIALS_DIRECTORY/forgejo_token"
    if [ ! -r "$credential" ]; then
      echo "forgejo-mcp: missing systemd credential forgejo_token" >&2
      exit 1
    fi

    export FORGEJO_ACCESS_TOKEN="$(${pkgs.coreutils}/bin/cat "$credential")"
    exec ${pkgs.forgejo-mcp}/bin/forgejo-mcp \
      --transport http \
      --http-port ${toString forgejoMcpPort} \
      --url http://127.0.0.1:3010
  '';
in
lib.recursiveUpdate (rec {
  nixpkgs = pkgsOptions;
  home.username = "siraben";
  home.homeDirectory = if isDarwin then "/Users/${home.username}" else "/home/${home.username}";
  home.packages = import ./packages.nix { inherit lib pkgs isDarwin isLinux profile; };

  home.sessionVariables = {
    EDITOR = "emacsclient";
    TZ = "America/Los_Angeles";
  } // (lib.optionalAttrs isDarwin {
    HOMEBREW_NO_AUTO_UPDATE = 1;
    HOMEBREW_NO_ANALYTICS = 1;
  });

  home.sessionPath = lib.optionals isDarwin [
    "/opt/homebrew/bin"
  ];

  home.language = {
    ctype = "en_US.UTF-8";
    base = "en_US.UTF-8";
  };

  home.file = lib.optionalAttrs isLinux {
    ".config/baloofilerc".text = ''
      [Basic Settings]
      Indexing-Enabled=false
    '';
  } // lib.optionalAttrs (profile == "headless") {
    ".agent-deck/config.toml".source =
      (pkgs.formats.toml { }).generate "agent-deck-config" {
        default_tool = "claude";
        theme = "dark";
        worktree.branch_prefix = "siraben/";
        worktree.default_location = "sibling";
        claude.dangerous_mode = true;
        feedback.disabled = true;
      };
  };

  programs = import ./programs.nix { inherit lib pkgs isDarwin isLinux profile; };
  fonts.fontconfig.enable = true;
  services = lib.optionalAttrs isLinux (import ./services.nix { inherit lib pkgs; });
  systemd.user.services.forgejo-mcp = lib.mkIf isLinux {
    Unit = {
      Description = "Forgejo MCP HTTP server";
    };

    Service = {
      LoadCredential = [ "forgejo_token:${forgejoMcpTokenPath}" ];
      ExecStart = "${forgejoMcpStart}";
      Restart = "on-failure";
      RestartSec = "10s";
    };

    Install = {
      WantedBy = [ "default.target" ];
    };
  };
  nix.package = lib.mkDefault pkgs.nix;
  nix.settings = {
    experimental-features = [ "nix-command" "flakes" ];
    keep-derivations = true;
    keep-outputs = true;
    builders-use-substitutes = true;
    plugin-files = "";
    substituters = [
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
      "https://siraben.cachix.org"
    ];
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "siraben.cachix.org-1:/zSVUB18DWcjQF52VMh0v7MzjI+pdevnWOa01koPoYc="
    ];
  };
  home.stateVersion = "25.05";
  home.enableNixpkgsReleaseCheck = false;
})
(lib.optionalAttrs (profile == "full") {
    home.activation.ensureKittySessionFile = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      session_file="$HOME/.config/kitty/sessions/last-session.conf"
      $DRY_RUN_CMD mkdir -p "$(dirname "$session_file")"
      if [ ! -s "$session_file" ]; then
        if [ -n "$DRY_RUN_CMD" ]; then
          $DRY_RUN_CMD printf 'new_tab\ncd ~\nlaunch zsh\n' \> "$session_file"
        else
          printf 'new_tab\ncd ~\nlaunch zsh\n' > "$session_file"
        fi
      fi
    '';

    home.file.".config/kitty/tab_bar.py".source = ./tab_bar.py;
})

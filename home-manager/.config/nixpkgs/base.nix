{ config, lib, currentSystem, profile, inputs, username ? "siraben", timeZone ? "America/Los_Angeles", ... }:

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
      # Keep Pi and its extensions on mutually compatible versions pinned in
      # this repository.
      (import ./pi.nix)
    ];
    config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) unfreePackages;
  };
  pkgs = import inputs.nixpkgs {
    system = currentSystem;
    inherit (pkgsOptions) overlays config;
  };
  manageClaudeSettingsModule = {
    options.siraben.manageClaudeSettings = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether Home Manager manages Claude Code settings";
    };
  };
in
lib.recursiveUpdate (rec {
  imports = [ ./pi-home.nix manageClaudeSettingsModule ];

  nixpkgs = pkgsOptions;
  home.username = lib.mkDefault username;
  home.homeDirectory = lib.mkDefault (
    if isDarwin then "/Users/${username}" else "/home/${username}"
  );
  home.packages = import ./packages.nix { inherit lib pkgs isDarwin isLinux profile; };

  home.sessionVariables = {
    EDITOR = "emacsclient";
    TZ = timeZone;
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

  home.file = {
    ".claude/hooks/block-find-nix-store.sh" = {
      executable = true;
      source = ./block-find-nix-store.sh;
    };
  } // lib.optionalAttrs config.siraben.manageClaudeSettings {
    ".claude/settings.json" = {
      force = true;
      source = ./claude-settings.json;
    };
  } // {
    ".codex/hooks/block-find-nix-store.sh" = {
      executable = true;
      source = ./block-find-nix-store.sh;
    };
    ".codex/hooks.json" = {
      force = true;
      source = ./codex-hooks.json;
    };
    ".codex/rules/custom.rules" = {
      force = true;
      source = ./codex-custom.rules;
    };
    # pi has no hooks.json; global extensions are auto-discovered here.
    ".pi/agent/extensions/block-expensive-scans.ts" = {
      force = true;
      source = ./pi-block-expensive-scans.ts;
    };
  } // lib.optionalAttrs isDarwin {
    "Library/Application Support/Code/User/settings.json" = {
      force = true;
      source = ./vscode-settings.json;
    };
  } // lib.optionalAttrs isLinux {
    ".config/baloofilerc".text = ''
      [Basic Settings]
      Indexing-Enabled=false
    '';
  };

  programs = import ./programs.nix { inherit lib pkgs isDarwin isLinux profile; };
  fonts.fontconfig.enable = true;
  services = lib.optionalAttrs isLinux (import ./services.nix { inherit lib pkgs; });
  # Determinate Nix owns the system installation and configuration on macOS.
  # Enabling this module there puts upstream Nix in the activation PATH, which
  # warns about Determinate-only settings such as eval-cores and lazy-trees.
  nix.enable = !isDarwin;
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
  xdg.configFile."nix/nix.conf" = lib.mkIf isDarwin {
    text = ''
      builders-use-substitutes = true
      experimental-features = nix-command flakes
      keep-derivations = true
      keep-outputs = true
      plugin-files =
      substituters = https://cache.nixos.org https://nix-community.cachix.org https://siraben.cachix.org
      trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs= siraben.cachix.org-1:/zSVUB18DWcjQF52VMh0v7MzjI+pdevnWOa01koPoYc=
    '';
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

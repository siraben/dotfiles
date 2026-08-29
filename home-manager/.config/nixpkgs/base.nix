{ config, lib, currentSystem, profile, inputs, timeZone ? "America/Los_Angeles", ... }:

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

  # Codex mutates config.toml with project trust, plugin, and app state, so
  # keep the file writable and merge our declarative settings into it.
  home.activation.mergeCodexConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    codex_config="$HOME/.codex/config.toml"
    codex_config_dir="$(dirname "$codex_config")"
    managed_config=${./codex-config.toml}

    if [ -n "$DRY_RUN_CMD" ]; then
      echo "Would merge $managed_config into $codex_config"
    else
      mkdir -p "$codex_config_dir"
      if [ -f "$codex_config" ]; then
        merged_config="$(${pkgs.coreutils}/bin/mktemp "$codex_config_dir/config.toml.XXXXXX")"
        managed_setting="$(${pkgs.gnugrep}/bin/grep -E \
          '^[[:space:]]*disable_paste_burst[[:space:]]*=' \
          "$managed_config")"
        ${pkgs.gawk}/bin/awk -v managed_setting="$managed_setting" '
          BEGIN { inserted = 0 }
          /^[[:space:]]*disable_paste_burst[[:space:]]*=/ {
            if (!inserted) print managed_setting
            inserted = 1
            next
          }
          !inserted && /^[[:space:]]*\[/ {
            print managed_setting
            print ""
            inserted = 1
          }
          { print }
          END {
            if (!inserted) {
              if (NR > 0) print ""
              print managed_setting
            }
          }
        ' "$codex_config" > "$merged_config"
        chmod --reference="$codex_config" "$merged_config"
        if ${pkgs.diffutils}/bin/cmp --silent "$codex_config" "$merged_config"; then
          rm "$merged_config"
        else
          mv "$merged_config" "$codex_config"
        fi
      else
        cp "$managed_config" "$codex_config"
        chmod u+w "$codex_config"
      fi
    fi
  '';

  home.file = {
    ".claude/hooks/block-find-nix-store.sh" = {
      executable = true;
      source = ./block-find-nix-store.sh;
    };
    ".claude/settings.json" = {
      force = true;
      source = ./claude-settings.json;
    };
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
    # pi has no hooks.json; the equivalent guard is a local extension
    # registered via the "extensions" array in ~/.pi/agent/settings.json.
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

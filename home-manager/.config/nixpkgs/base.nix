{
  lib,
  currentSystem,
  profile,
  inputs,
  username ? "siraben",
  timeZone ? "America/Los_Angeles",
  ...
}:

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
    "context-mode"
  ];
  pkgsOptions = {
    overlays = [
      inputs.siraben-overlay.overlays.default
      inputs.llm-agents.overlays.shared-nixpkgs
      (import ./overlay.nix { inherit inputs; })
      inputs.pi.overlays.default
      # Keep Pi extensions on mutually compatible versions pinned in this
      # repository while using Pi's upstream Nix package.
      (import ./pi.nix { inherit inputs; })
      (import ./cua-driver.nix)
    ];
    config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) unfreePackages;
  };
  pkgs = import inputs.nixpkgs {
    system = currentSystem;
    inherit (pkgsOptions) overlays config;
  };
in
lib.recursiveUpdate (
  (import ./nix-settings.nix { inherit lib pkgs isDarwin; })
  // {
    imports = [ ./pi-home.nix ];

    nixpkgs = pkgsOptions;
    home.username = lib.mkDefault username;
    home.homeDirectory = lib.mkDefault (if isDarwin then "/Users/${username}" else "/home/${username}");
    home.packages = import ./packages.nix {
      inherit
        lib
        pkgs
        isDarwin
        isLinux
        profile
        ;
    };

    home.sessionVariables = {
      EDITOR = "emacsclient";
      TZ = timeZone;
    }
    // (lib.optionalAttrs isDarwin {
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
      ".codex/skills/render-tex-pdf" = {
        force = true;
        source = ./skills/render-tex-pdf;
      };
      ".claude/skills/render-tex-pdf" = {
        force = true;
        source = ./skills/render-tex-pdf;
      };
      # pi has no hooks.json; global extensions are auto-discovered here.
      ".pi/agent/extensions/block-expensive-scans.ts" = {
        force = true;
        source = ./pi-block-expensive-scans.ts;
      };
      ".pi/agent/extensions/codex-usage.ts" = {
        force = true;
        source = ./pi-codex-usage.ts;
      };
    }
    // lib.optionalAttrs isDarwin {
      "Library/Application Support/Code/User/settings.json" = {
        force = true;
        source = ./vscode-settings.json;
      };
    }
    // lib.optionalAttrs isLinux {
      ".config/baloofilerc".text = ''
        [Basic Settings]
        Indexing-Enabled=false
      '';
    };

    programs = import ./programs.nix {
      inherit
        lib
        pkgs
        isDarwin
        isLinux
        profile
        ;
    };
    fonts.fontconfig.enable = true;
    services = lib.optionalAttrs isLinux (import ./services.nix { inherit lib pkgs; });
    home.stateVersion = "25.05";
    home.enableNixpkgsReleaseCheck = false;
  }
) (import ./programs/kitty-home.nix { inherit lib profile; })

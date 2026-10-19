{
  lib,
  pkgs,
  currentSystem,
  profile,
  inputs,
  username ? "siraben",
  timeZone ? "America/New_York",
  ...
}:

let
  inherit (lib.systems.elaborate { system = currentSystem; }) isLinux isDarwin;
  editor = if profile == "full" then "emacsclient" else "vim";
  pkgsOptions = {
    overlays = [
      inputs.siraben-overlay.overlays.default
      inputs.llm-agents.overlays.shared-nixpkgs
      (import ./overlay.nix { inherit inputs; })
      inputs.pi.overlays.default
      # Keep Pi extensions on mutually compatible versions pinned in this
      # repository while using Pi's upstream Nix package.
      (import ./pi/overlay.nix { inherit inputs; })
    ];
  };
in
lib.recursiveUpdate (rec {
  imports = [
    ./pi/modules
    ./nixpkgs-policy.nix
    ./nix-platform.nix
  ];
  # Use Home Manager's pkgs throughout so downstream overlays also apply to
  # the packages and programs selected by this profile.
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
    EDITOR = editor;
    VISUAL = editor;
    TZ = lib.mkDefault timeZone;
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
    ".claude/hooks/block-find-nix-store.py".source = ./block-find-nix-store.py;
    ".claude/hooks/block-find-nix-store.sh" = {
      executable = true;
      source = ./block-find-nix-store.sh;
    };
    ".codex/hooks/block-find-nix-store.py".source = ./block-find-nix-store.py;
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
}) (import ./programs/kitty-home.nix { inherit lib profile; })

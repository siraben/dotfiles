{ config, pkgs, ... }:

{
  # Home Manager configuration for siraben
  home.username = "siraben";
  home.homeDirectory = "/home/siraben";
  home.stateVersion = "24.11";

  # Let Home Manager manage itself
  programs.home-manager.enable = true;

  # Git configuration
  programs.git = {
    enable = true;
    userName = "Ben Siraphob";
    userEmail = "bensiraphob@gmail.com";
    extraConfig = {
      init.defaultBranch = "main";
      pull.rebase = true;
      push.autoSetupRemote = true;
    };
  };

  # Zsh configuration
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;
    
    oh-my-zsh = {
      enable = true;
      theme = "robbyrussell";
      plugins = [ "git" "docker" "kubectl" "z" ];
    };
    
    shellAliases = {
      ll = "ls -l";
      la = "ls -la";
      ls = "eza";
      cat = "bat";
      grep = "ripgrep";
      find = "fd";
      
      # NixOS aliases
      nrs = "sudo nixos-rebuild switch";
      nrb = "sudo nixos-rebuild boot";
      nrt = "sudo nixos-rebuild test";
      nru = "sudo nixos-rebuild switch --upgrade";
      
      # Git aliases
      gs = "git status";
      ga = "git add";
      gc = "git commit";
      gp = "git push";
      gl = "git log --oneline --graph";
      gd = "git diff";
    };
  };

  # Starship prompt
  programs.starship = {
    enable = true;
    enableZshIntegration = true;
  };

  # Neovim configuration
  programs.neovim = {
    enable = true;
    defaultEditor = true;
    viAlias = true;
    vimAlias = true;
    
    plugins = with pkgs.vimPlugins; [
      vim-nix
      vim-fugitive
      nerdtree
      vim-airline
      vim-airline-themes
      vim-sensible
      vim-surround
      vim-commentary
    ];
    
    extraConfig = ''
      set number
      set relativenumber
      set expandtab
      set tabstop=2
      set shiftwidth=2
      set smartindent
      set ignorecase
      set smartcase
      set hlsearch
      set incsearch
      
      " NERDTree
      nnoremap <C-n> :NERDTreeToggle<CR>
      
      " Clear search highlight
      nnoremap <leader><space> :nohlsearch<CR>
    '';
  };

  # Tmux configuration
  programs.tmux = {
    enable = true;
    clock24 = true;
    plugins = with pkgs.tmuxPlugins; [
      sensible
      yank
      resurrect
      continuum
    ];
    extraConfig = ''
      # Set prefix to Ctrl-a
      unbind C-b
      set-option -g prefix C-a
      bind-key C-a send-prefix
      
      # Split panes using | and -
      bind | split-window -h
      bind - split-window -v
      unbind '"'
      unbind %
      
      # Reload config
      bind r source-file ~/.tmux.conf \; display-message "Config reloaded!"
      
      # Enable mouse
      set -g mouse on
      
      # Don't rename windows automatically
      set-option -g allow-rename off
    '';
  };

  # VS Code configuration
  programs.vscode = {
    enable = true;
    extensions = with pkgs.vscode-extensions; [
      ms-python.python
      ms-vscode.cpptools
      rust-lang.rust-analyzer
      golang.go
      jnoortheen.nix-ide
      ms-azuretools.vscode-docker
      github.copilot
      eamodio.gitlens
      esbenp.prettier-vscode
    ];
    userSettings = {
      "editor.fontSize" = 14;
      "editor.fontFamily" = "'JetBrains Mono', 'Fira Code', monospace";
      "editor.fontLigatures" = true;
      "editor.formatOnSave" = true;
      "editor.minimap.enabled" = false;
      "editor.rulers" = [ 80 120 ];
      "files.autoSave" = "afterDelay";
      "workbench.colorTheme" = "Default Dark+";
      "terminal.integrated.fontSize" = 14;
    };
  };

  # Firefox configuration
  programs.firefox = {
    enable = true;
    profiles.siraben = {
      settings = {
        "browser.startup.homepage" = "about:home";
        "browser.search.region" = "US";
        "browser.search.isUS" = true;
        "distribution.searchplugins.defaultLocale" = "en-US";
        "general.useragent.locale" = "en-US";
        "privacy.trackingprotection.enabled" = true;
        "privacy.trackingprotection.socialtracking.enabled" = true;
      };
    };
  };

  # Direnv for automatic environment loading
  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };

  # FZF for fuzzy finding
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  # bat configuration
  programs.bat = {
    enable = true;
    config = {
      theme = "TwoDark";
      number = true;
    };
  };

  # eza (better ls)
  programs.eza = {
    enable = true;
    enableZshIntegration = true;
    icons = "auto";
  };

  # zoxide (better cd)
  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };

  # KDE/Plasma configuration
  home.packages = with pkgs; [
    # Additional user packages
    bitwarden
    signal-desktop
    telegram-desktop
    spotify
    zoom-us
    
    # Development tools
    docker-compose
    kubectl
    k9s
    terraform
    ansible
    
    # Utilities
    jq
    yq
    httpie
    tldr
    ncdu
    duf
    procs
    bottom
  ];

  # SSH client configuration
  programs.ssh = {
    enable = true;
    compression = true;
    serverAliveInterval = 60;
    
    matchBlocks = {
      "github.com" = {
        hostname = "github.com";
        user = "git";
      };
    };
  };

  # GPG configuration
  programs.gpg = {
    enable = true;
  };
  
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    pinentryPackage = pkgs.pinentry-qt;
  };

  # Desktop files
  xdg.enable = true;
  xdg.configFile = {
    "fontconfig/conf.d/10-nix-fonts.conf".text = ''
      <?xml version="1.0"?>
      <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
      <fontconfig>
        <alias>
          <family>monospace</family>
          <prefer>
            <family>JetBrains Mono</family>
            <family>Fira Code</family>
          </prefer>
        </alias>
      </fontconfig>
    '';
  };
}
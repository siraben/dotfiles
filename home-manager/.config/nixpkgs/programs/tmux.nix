{ }:

{
  tmux = {
    enable = true;
    clock24 = true;
    baseIndex = 1;
    historyLimit = 200000;
    mouse = true;
    terminal = "tmux-256color";
    # tmux defaults and key bindings shared across hosts
    extraConfig = ''
      # Truecolor / 24-bit color
      set -as terminal-overrides ",xterm-256color:Tc,xterm-kitty:Tc,tmux*:Tc"

      # Forward modified keys such as Shift+Enter and Alt+Enter to Pi.
      set -g extended-keys on
      set -g extended-keys-format csi-u

      # Quality-of-life
      set -g  focus-events on
      set -g  renumber-windows on
      set -g  pane-base-index 1
      set -g  detach-on-destroy off
      set -g  status-interval 5
      set -g  display-panes-time 2000

      # Open new windows/panes in the current working directory
      bind c new-window -c "#{pane_current_path}"
      bind '"' split-window -c "#{pane_current_path}"
      bind % split-window -h -c "#{pane_current_path}"

      # Session defaults
      set -g allow-rename off
      set -g set-titles on
      set -g set-titles-string '#{host_short} · #S'

      # Catppuccin Mocha theme — minimal, default fg throughout
      set -g status-position bottom
      set -g status-justify left
      set -g status-style "bg=default,fg=default"
      set -g status-left-length 60
      set -g status-right-length 100

      set -g pane-border-style 'fg=#313244'
      set -g pane-active-border-style 'fg=#89b4fa'

      set -g message-style "bg=#313244,fg=default"
      set -g message-command-style "bg=#313244,fg=default"
      set -g mode-style "bg=#45475a,fg=default"

      set -g status-left "#[fg=#1e1e2e,bg=#89b4fa,bold]  #S #[fg=#89b4fa,bg=default,nobold] "
      set -g status-right "#[fg=#6c7086]#{b:pane_current_path} "

      setw -g window-status-format         "#[fg=default,bg=default] #I #W "
      setw -g window-status-current-format "#[fg=default,bg=#313244,bold] #I #W #[bg=default,nobold]"

      setw -g window-status-activity-style "fg=#f38ba8,bg=default"
      setw -g window-status-bell-style "fg=#f38ba8,bg=default,bold"
      setw -g window-status-separator " "
      setw -g clock-mode-colour "#89b4fa"
    '';
  };
}

{ lib, profile }:

lib.optionalAttrs (profile == "full") {

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

  home.file.".config/kitty/tab_bar.py".source = ../tab_bar.py;
}

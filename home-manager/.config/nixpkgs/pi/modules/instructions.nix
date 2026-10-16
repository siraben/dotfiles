{ config, lib, ... }: {
  config = lib.mkIf config.siraben.pi.enable {
    home.file."${config.programs.pi-coding-agent.configDir}/AGENTS.md".force = true;
    programs.pi-coding-agent.context = ''
      # Background work
      - `bg_run` tasks and async `subagent` runs send a completion notification that wakes this session. After launching one, end the turn.
      - `bg_wait` is for `subagent` runs only; `bg_run` tasks are covered by their notification.
    '';
  };
}

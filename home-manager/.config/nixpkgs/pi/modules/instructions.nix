{ config, lib, ... }: {
  config = lib.mkIf config.siraben.pi.enable {
    home.file."${config.programs.pi-coding-agent.configDir}/AGENTS.md".force = true;
    programs.pi-coding-agent.context = ''
      # Background work
      - Ordinary async subagents notify and wake this session when they finish. After launching one, end the turn instead of polling or calling `bg_wait` merely because it is active.
      - `bg_wait` tracks subagents and registered provider work; it cannot wait for `bg_task_*` jobs.
      - When a `bg_task_*` result is needed, end the turn: its completion notice wakes this session. Do not use foreground `sleep` or polling to wait for it.
      - Use `bg_task_watch` to check repeatedly until a condition holds.
    '';
  };
}

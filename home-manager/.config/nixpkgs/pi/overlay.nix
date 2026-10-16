{ inputs }:
final: _: {
  pi-subagents = import ./packages/pi-subagents.nix {
    pkgs = final;
    inherit inputs;
  };
  pi-tool-summaries = import ./packages/pi-tool-summaries.nix {
    pkgs = final;
    inherit inputs;
  };
  pi-background-tasks = import ./packages/pi-background-tasks.nix {
    pkgs = final;
    inherit inputs;
  };
  pi-codex-goal = import ./packages/pi-codex-goal.nix {
    pkgs = final;
    inherit inputs;
  };
  pi-web-access = import ./packages/pi-web-access.nix {
    pkgs = final;
    inherit inputs;
  };
  context-mode = import ./packages/context-mode.nix {
    pkgs = final;
    inherit inputs;
  };
  cua-driver = import ./packages/cua-driver.nix { pkgs = final; };
}

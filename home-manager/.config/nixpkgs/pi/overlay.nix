{ inputs }:
final: _prev: {
  piPackages = import ./package-set.nix {
    pkgs = final;
    inherit inputs;
  };

  # Top-level aliases for the scoped packages.
  inherit (final.piPackages) buildPiExtension buildPiNpmExtension cua-driver;
  pi-subagents = final.piPackages.subagents;
  pi-tool-summaries = final.piPackages.tool-summaries;
  pi-background-tasks = final.piPackages.background-tasks;
  pi-codex-goal = final.piPackages.codex-goal;
  pi-web-access = final.piPackages.web-access;
  pi-context-mode = final.piPackages.context-mode;
}

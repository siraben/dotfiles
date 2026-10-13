{ inputs }:
final: _prev:
let
  buildSupport = final.callPackage ./build-support.nix { };
in
{
  inherit (buildSupport) buildPiExtension buildPiNpmExtension;

  pi-subagents = final.callPackage ./packages/pi-subagents.nix {
    src = inputs.pi-subagents;
    inherit (final) pi;
  };

  pi-tool-summaries = final.callPackage ./packages/pi-tool-summaries.nix {
    src = inputs.pi-tool-summaries;
  };

  pi-background-tasks = import ./packages/pi-background-tasks.nix {
    pkgs = final;
  };

  pi-codex-goal = final.callPackage ./packages/pi-codex-goal.nix {
    src = inputs.pi-codex-goal;
  };

  pi-web-access = final.callPackage ./packages/pi-web-access.nix {
    src = inputs.pi-web-access;
  };

  pi-context-mode = final.callPackage ./packages/pi-context-mode.nix {
    src = inputs.context-mode;
  };

  cua-driver = final.callPackage ./packages/cua-driver.nix { };
}

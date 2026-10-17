{ pkgs, inputs }:
pkgs.lib.makeScope pkgs.newScope (self: {
  # Keep the host, native-addon ABI, and SDK layout together. Consumers using
  # another Pi distribution can replace this descriptor with overrideScope.
  runtime = {
    package = pkgs.pi;
    nodejs = pkgs.nodejs_22;
    sdkRoot = "${self.runtime.package}/lib/pi/node_modules";
  };
  nodejs = self.runtime.nodejs;

  inherit (self.callPackage ./build-support.nix { }) buildPiExtension buildPiNpmExtension;

  subagents = self.callPackage ./packages/pi-subagents.nix { src = inputs.pi-subagents; };
  tool-summaries = self.callPackage ./packages/pi-tool-summaries.nix {
    src = inputs.pi-tool-summaries;
  };
  background-tasks = self.callPackage ./packages/pi-background-tasks.nix { };
  codex-goal = self.callPackage ./packages/pi-codex-goal.nix { src = inputs.pi-codex-goal; };
  web-access = self.callPackage ./packages/pi-web-access.nix { src = inputs.pi-web-access; };
  context-mode = self.callPackage ./packages/pi-context-mode.nix { src = inputs.context-mode; };
  cua-driver = self.callPackage ./packages/cua-driver.nix { };
})

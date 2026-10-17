{ pkgs, inputs }:
pkgs.lib.makeScope pkgs.newScope (self: {
  # Keep the host and native-addon ABI together. Published extensions resolve
  # Pi peers through the host rather than linking its private installation tree.
  runtime = {
    package = pkgs.pi;
    nodejs = pkgs.nodejs_22;
  };
  nodejs = self.runtime.nodejs;
  # importNpmLock's hook embeds Node headers and node-gyp at construction time;
  # selecting Node only on buildNpmPackage would leave those on nixpkgs's default.
  importNpmLock = pkgs.callPackage "${pkgs.path}/pkgs/build-support/node/import-npm-lock" {
    callPackages = path: args: pkgs.callPackages path (args // { inherit (self) nodejs; });
  };

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

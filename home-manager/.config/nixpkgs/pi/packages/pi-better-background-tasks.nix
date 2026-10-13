{ pkgs, inputs }:
let
  src = inputs.pi-better-harness + "/packages/pi-better-background-tasks";
  manifest = builtins.fromJSON (builtins.readFile "${src}/package.json");
  lock = builtins.fromJSON (builtins.readFile "${inputs.pi-better-harness}/package-lock.json");
  # Preserve npm's nested retry version, rather than the workspace's top-level retry.
  runtimePaths = [
    "node_modules/proper-lockfile"
    "node_modules/graceful-fs"
    "node_modules/proper-lockfile/node_modules/retry"
    "node_modules/signal-exit"
  ];
  installRuntime =
    path:
    let
      entry = lock.packages.${path};
      tarball = pkgs.fetchurl {
        # Use the public mirror for networks that restrict registry.npmjs.org.
        url =
          builtins.replaceStrings [ "https://registry.npmjs.org/" ] [ "https://registry.yarnpkg.com/" ]
            entry.resolved;
        hash = entry.integrity;
      };
    in
    ''
      mkdir -p "$dir/${path}"
      tar -xzf ${tarball} -C "$dir/${path}" --strip-components=1
    '';
in
pkgs.stdenvNoCC.mkDerivation {
  pname = "pi-better-background-tasks";
  inherit src;
  inherit (manifest) version;

  # SSH polling uses the remote login shell; zsh reserves `status` as read-only.
  patches = [ ../../pi-better-background-tasks-zsh.patch ];

  dontBuild = true;

  installPhase = ''
    runHook preInstall

    dir="$out/lib/node_modules/pi-better-background-tasks"
    mkdir -p "$dir"
    cp -R . "$dir/"
    ${pkgs.lib.concatMapStrings installRuntime runtimePaths}

    runHook postInstall
  '';

  meta = with pkgs.lib; {
    description = "Durable background shell tasks, watchers, and logs for Pi";
    homepage = "https://github.com/1aboveio/pi-better-harness/tree/main/packages/pi-better-background-tasks";
    license = licenses.mit;
  };
}

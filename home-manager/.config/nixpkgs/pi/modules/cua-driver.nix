{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.siraben.pi;
  cuaDriverEnvironment = import ../cua-environment.nix;
in
{
  config = lib.mkIf (cfg.enable && cfg.enableCuaDriver && pkgs.stdenv.hostPlatform.isDarwin) {
    home.packages = [ pkgs.cua-driver ];
    home.sessionVariables = cuaDriverEnvironment;
    launchd.agents.cua-driver = {
      enable = true;
      config = {
        ProgramArguments = [
          "${pkgs.cua-driver}/libexec/CuaDriver.app/Contents/MacOS/cua-driver"
          "serve"
        ];
        EnvironmentVariables = cuaDriverEnvironment;
        RunAtLoad = true;
        KeepAlive = true;
        ProcessType = "Interactive";
      };
    };
  };
}

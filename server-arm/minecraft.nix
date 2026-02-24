# On-demand Minecraft server module
#
# Uses lazymc to proxy the public Minecraft port.  When a whitelisted
# client connects, lazymc starts a containerised Minecraft server via
# podman and proxies traffic to it.  After an idle timeout the server
# is stopped automatically.

{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.ondemand-minecraft;
  proxy = cfg.proxy;
  portStr = toString cfg.port;
  backendPort = cfg.port + 1;
  backendPortStr = toString backendPort;

  # Environment variables passed to the itzg/minecraft-server container.
  allEnv = {
    EULA = "TRUE";
    MEMORY = cfg.memory;
  } // cfg.extraEnvironment;

  # Shell script that lazymc invokes as its server.command.
  # exec replaces the shell so lazymc can signal podman directly.
  startScript =
    let
      envArgs = lib.concatMap (k: [ "-e" "${k}=${allEnv.${k}}" ]) (lib.attrNames allEnv);
      envFileArgs = lib.concatMap (f: [ "--env-file" (toString f) ]) cfg.environmentFiles;
      volumeArgs = lib.concatMap (v: [ "-v" v ]) ([ "${cfg.dataDir}:/data" ] ++ cfg.extraVolumes);
      args =
        [
          "${pkgs.podman}/bin/podman"
          "run"
          "--rm"
          "--replace"
          "--name"
          "minecraft"
        ]
        ++ envArgs
        ++ envFileArgs
        ++ [
          "-p"
          "127.0.0.1:${backendPortStr}:25565"
        ]
        ++ volumeArgs
        ++ [ cfg.image ];
    in
    pkgs.writeShellScript "minecraft-start" ''
      exec ${lib.escapeShellArgs args}
    '';

  joinMethodsStr = lib.concatMapStringsSep ", " (m: ''"${m}"'') proxy.joinMethods;

  lazymcConfig = pkgs.writeText "lazymc.toml" ''
    [public]
    address = "0.0.0.0:${portStr}"
    version = "${proxy.version}"
    protocol = ${toString proxy.protocol}

    [server]
    address = "127.0.0.1:${backendPortStr}"
    directory = "${cfg.dataDir}"
    command = "${startScript}"
    wake_whitelist = ${lib.boolToString proxy.wakeWhitelist}
    wake_on_start = ${lib.boolToString proxy.wakeOnStart}
    wake_on_crash = ${lib.boolToString proxy.wakeOnCrash}

    [time]
    sleep_after = ${toString (cfg.idleShutdownMinutes * 60)}
    minimum_online_time = ${toString (cfg.startupGraceMinutes * 60)}

    [join]
    methods = [${joinMethodsStr}]

    [join.kick]
    starting = "${proxy.kickStartingMessage}"

    [motd]
    sleeping = "${proxy.motdSleeping}"
    starting = "${proxy.motdStarting}"

    [advanced]
    rewrite_server_properties = ${lib.boolToString proxy.rewriteServerProperties}
  '';
in
{
  options.services.ondemand-minecraft = {
    enable = lib.mkEnableOption "on-demand Minecraft server";

    port = lib.mkOption {
      type = lib.types.port;
      default = 25565;
      description = "TCP port the Minecraft server listens on.";
    };

    memory = lib.mkOption {
      type = lib.types.str;
      default = "4G";
      description = "JVM heap size (e.g. \"4G\", \"2048M\").";
    };

    image = lib.mkOption {
      type = lib.types.str;
      default = "itzg/minecraft-server";
      description = "OCI image for the Minecraft server.";
    };

    dataDir = lib.mkOption {
      type = lib.types.path;
      default = "/home/minecraft";
      description = "Host path mounted as /data in the container.";
    };

    extraEnvironment = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      description = "Extra environment variables passed to the itzg image.";
    };

    extraVolumes = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Extra volume mounts for the container.";
    };

    environmentFiles = lib.mkOption {
      type = lib.types.listOf lib.types.path;
      default = [ ];
      description = "Environment files passed to the container (KEY=VALUE format).";
    };

    idleShutdownMinutes = lib.mkOption {
      type = lib.types.int;
      default = 20;
      description = "Minutes of no connections before the server shuts down.";
    };

    startupGraceMinutes = lib.mkOption {
      type = lib.types.int;
      default = 10;
      description = "Minutes to wait for the first connection after startup.";
    };

    proxy = {
      wakeWhitelist = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Only allow whitelisted players (from whitelist.json in dataDir) to wake the server.";
      };

      wakeOnStart = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Start the Minecraft server immediately when lazymc launches.";
      };

      wakeOnCrash = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Automatically restart the Minecraft server after a crash.";
      };

      rewriteServerProperties = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Let lazymc rewrite server.properties to set the correct port and RCON settings.";
      };

      joinMethods = lib.mkOption {
        type = lib.types.listOf (lib.types.enum [ "kick" "hold" "forward" "lobby" ]);
        default = [ "hold" "kick" ];
        description = "Ordered list of methods to handle players connecting while the server is down.";
      };

      kickStartingMessage = lib.mkOption {
        type = lib.types.str;
        default = "Server is starting...\\n\\nPlease try again in a minute.";
        description = "Disconnect message shown when a player is kicked while the server starts.";
      };

      motdSleeping = lib.mkOption {
        type = lib.types.str;
        default = "Server is sleeping\\n\\u00a72Join to wake it up";
        description = "MOTD shown in the server list when the server is sleeping.";
      };

      motdStarting = lib.mkOption {
        type = lib.types.str;
        default = "\\u00a72Server is starting...\\n\\u00a77Please wait";
        description = "MOTD shown in the server list when the server is starting.";
      };

      version = lib.mkOption {
        type = lib.types.str;
        default = "1.21.1.1";
        description = "Minecraft version string shown in the server list before the server is probed.";
      };

      protocol = lib.mkOption {
        type = lib.types.int;
        default = 774;
        description = "Minecraft protocol version matching the version string (e.g. 767 for 1.21.1).";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    virtualisation.podman.enable = true;

    # Ensure data directory exists with itzg container UID/GID (1000:1000)
    systemd.tmpfiles.rules = [ "d ${cfg.dataDir} 0755 1000 1000 -" ];

    systemd.services.minecraft-lazymc = {
      description = "Minecraft on-demand server (lazymc)";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.lazymc}/bin/lazymc -c ${lazymcConfig} start";
        Restart = "always";
        RestartSec = "5s";
        # Hardening (ProtectKernelTunables omitted: netavark needs sysctl writes)
        PrivateTmp = true;
        ProtectKernelModules = true;
      };
    };

    # Open Minecraft port
    networking.firewall.allowedTCPPorts = [ cfg.port ];
  };
}

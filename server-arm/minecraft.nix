# On-demand Minecraft server module
#
# Runs a containerized Minecraft server that starts automatically when
# a client connects and shuts down after a configurable idle period.
# Uses systemd socket-style activation via ncat + a watchdog service.

{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.ondemand-minecraft;
  portStr = toString cfg.port;
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
  };

  config = lib.mkIf cfg.enable {
    # Podman for OCI containers
    virtualisation.podman.enable = true;
    virtualisation.oci-containers.backend = "podman";

    # Minecraft server container (started on-demand, not auto-started)
    virtualisation.oci-containers.containers.minecraft = {
      image = cfg.image;
      environment = {
        EULA = "TRUE";
        MEMORY = cfg.memory;
      } // cfg.extraEnvironment;
      ports = [ "${portStr}:${portStr}" ];
      volumes = [ "${cfg.dataDir}:/data" ];
    };

    # Override: don't auto-start the minecraft container
    systemd.services.podman-minecraft.wantedBy = lib.mkForce [ ];

    # TCP listener that starts minecraft on incoming connection
    # Uses systemd socket activation: listens on the port, starts minecraft,
    # then gets out of the way once the real server is up.
    systemd.services.minecraft-listener = {
      description = "Minecraft on-demand TCP listener";
      wantedBy = [ "multi-user.target" ];
      # Only run when minecraft is stopped
      conflicts = [ "podman-minecraft.service" ];
      after = [ "network.target" ];
      script = ''
        echo "Waiting for connection on port ${portStr}..."
        ${pkgs.nmap}/bin/ncat -l -p ${portStr} -w 1 2>/dev/null || true
        echo "Connection received, starting Minecraft server..."
        systemctl start podman-minecraft.service
      '';
      serviceConfig = {
        Type = "oneshot";
        Restart = "on-failure";
        RestartSec = "5s";
      };
    };

    # Restart the listener whenever minecraft stops
    systemd.services.minecraft-listener-restart = {
      description = "Restart Minecraft listener after server stops";
      after = [ "podman-minecraft.service" ];
      script = ''
        # Small delay so the port is released
        sleep 2
        systemctl start minecraft-listener.service
      '';
      serviceConfig.Type = "oneshot";
    };

    # Trigger listener restart when minecraft stops
    systemd.paths.minecraft-listener-watch = {
      wantedBy = [ "multi-user.target" ];
    };

    # Watchdog: checks for idle connections and shuts down after timeout
    systemd.services.minecraft-watchdog = {
      description = "Minecraft idle shutdown watchdog";
      after = [ "podman-minecraft.service" ];
      bindsTo = [ "podman-minecraft.service" ];
      script = ''
        IDLE_MINUTES=0
        SHUTDOWN_AFTER=${toString cfg.idleShutdownMinutes}
        STARTUP_GRACE=${toString cfg.startupGraceMinutes}

        echo "Watchdog started. Waiting $STARTUP_GRACE min for first connection..."

        # Startup grace period: wait for first connection
        MINUTE=0
        while [ $MINUTE -lt $STARTUP_GRACE ]; do
          sleep 60
          MINUTE=$((MINUTE + 1))
          CONNECTIONS=$(${pkgs.iproute2}/bin/ss -tn state established '( dport = :${portStr} or sport = :${portStr} )' | tail -n +2 | wc -l)
          if [ "$CONNECTIONS" -gt 0 ]; then
            echo "Connection detected during startup grace period."
            break
          fi
          echo "No connections yet, minute $MINUTE of $STARTUP_GRACE..."
        done

        # Check if anyone connected during grace period
        CONNECTIONS=$(${pkgs.iproute2}/bin/ss -tn state established '( dport = :${portStr} or sport = :${portStr} )' | tail -n +2 | wc -l)
        if [ "$CONNECTIONS" -lt 1 ]; then
          echo "No connections after $STARTUP_GRACE minutes, shutting down."
          systemctl stop podman-minecraft.service
          exit 0
        fi

        echo "Switching to idle shutdown watcher ($SHUTDOWN_AFTER min timeout)..."

        # Idle shutdown loop
        while true; do
          sleep 60
          CONNECTIONS=$(${pkgs.iproute2}/bin/ss -tn state established '( dport = :${portStr} or sport = :${portStr} )' | tail -n +2 | wc -l)
          if [ "$CONNECTIONS" -lt 1 ]; then
            IDLE_MINUTES=$((IDLE_MINUTES + 1))
            echo "No connections, idle for $IDLE_MINUTES of $SHUTDOWN_AFTER minutes..."
            if [ $IDLE_MINUTES -ge $SHUTDOWN_AFTER ]; then
              echo "$SHUTDOWN_AFTER minutes idle, shutting down Minecraft."
              systemctl stop podman-minecraft.service
              exit 0
            fi
          else
            if [ $IDLE_MINUTES -gt 0 ]; then
              echo "Connection detected, resetting idle counter."
            fi
            IDLE_MINUTES=0
          fi
        done
      '';
      serviceConfig = {
        Type = "simple";
        Restart = "no";
      };
    };

    # Open Minecraft port
    networking.firewall.allowedTCPPorts = [ cfg.port ];
  };
}

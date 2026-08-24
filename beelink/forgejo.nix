# Forgejo replica adapted from
# https://github.com/DieracDelta/flakes/tree/tmp_no_mold (hosts/desktop.nixos.nix)
#
# Diffs vs source:
#   - domain → beelink's tailnet name
#   - homepage-dashboard / wger / koito / soulseek / etc. omitted (host-specific)
#   - postgresql added (justin had it elsewhere)
#   - caddy enabled (justin already had it on)

{ config, pkgs, lib, ... }:

let
  forgejoDomain = "beelink.tail09906.ts.net";
  forgejoBasePath = "/forgejo";
  forgejoPort = 3010;
in
{
  services.postgresql = {
    package = pkgs.postgresql_17;
    enable = true;
    ensureDatabases = [ "forgejo" ];
    ensureUsers = [
      {
        name = "forgejo";
        ensureDBOwnership = true;
      }
    ];
  };

  users.groups.gitea-runner = { };
  users.users.gitea-runner = {
    isSystemUser = true;
    group = "gitea-runner";
    extraGroups = [ "docker" ];
    home = "/var/cache/forgejo-actions/runner";
    createHome = true;
  };

  services.forgejo = {
    enable = true;
    package = pkgs.forgejo-lts;
    stateDir = "/var/lib/forgejo";

    database = {
      type = "postgres";
    };

    lfs.enable = true;

    dump = {
      enable = true;
      interval = "03:45";
      backupDir = "/var/lib/forgejo/dump";
      type = "tar.zst";
      age = "8w";
    };

    settings = {
      DEFAULT = {
        APP_NAME = "Forgejo";
      };

      server = {
        DOMAIN = forgejoDomain;
        ROOT_URL = "https://${forgejoDomain}${forgejoBasePath}/";
        HTTP_ADDR = "127.0.0.1";
        HTTP_PORT = forgejoPort;
        DISABLE_SSH = false;
        SSH_DOMAIN = forgejoDomain;
        SSH_PORT = 22;
      };

      session = {
        COOKIE_NAME = "forgejo_session";
        COOKIE_SECURE = true;
      };

      service = {
        DISABLE_REGISTRATION = false;
        REQUIRE_SIGNIN_VIEW = true;
      };

      mirror = {
        ENABLED = true;
        DEFAULT_INTERVAL = "8h";
        MIN_INTERVAL = "10m";
      };

      repository = {
        DEFAULT_REPO_UNITS = "repo.code,repo.releases,repo.issues,repo.pulls,repo.wiki,repo.projects,repo.packages,repo.actions";
      };

      "repository.pull-request" = {
        DEFAULT_MERGE_STYLE = "rebase";
      };

      actions = {
        ENABLED = true;
      };
    };
  };

  services.gitea-actions-runner = {
    package = pkgs.forgejo-runner;
    instances.beelink = {
      enable = true;
      name = "beelink";
      url = "http://127.0.0.1:${toString forgejoPort}";
      tokenFile = "/var/lib/forgejo/runner_token";
      labels = [
        "native:host"
        "ubuntu-latest:host"
        "ubuntu-22.04:host"
        "debian-latest:host"
      ];
      hostPackages = with pkgs; [
        bash coreutils curl findutils gawk gitMinimal gnused jq nix nodejs util-linux wget
      ];
      settings.log = {
        level = "debug";
        job_level = "debug";
      };
      settings.runner.capacity = 4;
      settings.container = {
        docker_host = "-";
        force_pull = false;
        force_rebuild = false;
        workdir_parent = "/var/cache/forgejo-actions/work";
      };
      settings.host.workdir_parent = "/var/cache/forgejo-actions/work";
      settings.cache = {
        enabled = true;
        dir = "/var/cache/forgejo-actions/runner/actcache";
      };
    };
    instances.beelink-docker = {
      enable = true;
      name = "beelink-docker";
      url = "http://127.0.0.1:${toString forgejoPort}";
      tokenFile = "/var/lib/forgejo/runner_token";
      labels = [
        "docker:docker://docker.io/nixos/nix:latest"
      ];
      hostPackages = with pkgs; [
        bash coreutils curl findutils gawk gitMinimal gnused jq nix nodejs util-linux wget
      ];
      settings.log = {
        level = "debug";
        job_level = "debug";
      };
      settings.runner.capacity = 4;
      settings.container = {
        docker_host = "unix:///run/docker.sock";
        force_pull = false;
        force_rebuild = false;
        network = "host";
        workdir_parent = "/var/cache/forgejo-actions/work";
      };
      settings.host.workdir_parent = "/var/cache/forgejo-actions/work";
      settings.cache = {
        enabled = true;
        dir = "/var/cache/forgejo-actions/runner/actcache";
      };
    };
  };

  systemd.services.gitea-runner-beelink = {
    after = [ "forgejo.service" ];
    requires = [ "forgejo.service" ];
    environment.HOME = lib.mkForce "/var/cache/forgejo-actions/runner";
    serviceConfig = {
      DynamicUser = lib.mkForce false;
      User = "gitea-runner";
      Group = "gitea-runner";
      WorkingDirectory = lib.mkForce "/var/lib/gitea-runner/beelink";
      ReadWritePaths = [
        "/var/cache/forgejo-actions"
        "/var/tmp"
      ];
    };
  };

  systemd.services."gitea-runner-beelink\\x2ddocker" = {
    environment.HOME = lib.mkForce "/var/cache/forgejo-actions/runner";
    after = [ "forgejo.service" ];
    requires = [ "forgejo.service" ];
    serviceConfig = {
      DynamicUser = lib.mkForce false;
      User = "gitea-runner";
      Group = "gitea-runner";
      WorkingDirectory = lib.mkForce "/var/lib/gitea-runner/beelink-docker";
      ReadWritePaths = [
        "/var/cache/forgejo-actions"
        "/var/tmp"
      ];
    };
  };

  systemd.tmpfiles.rules = [
    "a+ /home/jrestivo - - - - u:gitea-runner:--x"
    "Z /var/lib/gitea-runner 0755 gitea-runner gitea-runner -"
    "d /var/cache/forgejo-actions 0775 gitea-runner gitea-runner -"
    "d /var/cache/forgejo-actions/runner 0775 gitea-runner gitea-runner -"
    "d /var/cache/forgejo-actions/runner/actcache 0775 gitea-runner gitea-runner -"
    "d /var/cache/forgejo-actions/work 0775 gitea-runner gitea-runner -"
  ];

  # Let caddy invoke `tailscale cert` to obtain browser-trusted certs.
  services.tailscale.permitCertUid = "caddy";

  # Vanilla caddy is sufficient: caddy 2.7+ auto-detects *.ts.net domains
  # and shells out to `tailscale cert` (allowed via permitCertUid above) to
  # obtain a browser-trusted certificate — no plugin required.
  services.caddy = {
    enable = true;
    virtualHosts."${forgejoDomain}".extraConfig = ''
      redir ${forgejoBasePath} ${forgejoBasePath}/ permanent
      handle_path ${forgejoBasePath}/* {
        reverse_proxy 127.0.0.1:${toString forgejoPort}
      }
    '';
  };
}

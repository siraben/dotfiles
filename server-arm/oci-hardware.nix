# OCI ARM Hardware Module
#
# Hardware support for Oracle Cloud Infrastructure ARM instances
# (VM.Standard.A1.Flex / Ampere A1). Handles:
# - iSCSI boot via iBFT (iSCSI Boot Firmware Table)
# - Mellanox ConnectX-6 network driver
# - Required kernel modules and initrd configuration
# - LVM support for multi-volume setups
# - Optional initrd SSH for emergency access

{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.oci.hardware;
in
{
  options.oci.hardware = {
    enableLVM = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Enable LVM support in the initrd. Required for setups using LVM
        to combine boot volume and block volumes.
      '';
    };

    initrdSSH = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          Enable SSH access in the initrd for emergency debugging.
          Useful when LVM or other early-boot services fail.
        '';
      };

      authorizedKeys = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "SSH public keys authorized to access initrd SSH";
      };

      hostKeyFile = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        description = ''
          Path to the SSH host key file for initrd SSH.
          This should be generated once and stored securely.
        '';
      };
    };
  };

  config = {
    # Required for iSCSI boot with networkd
    networking.useNetworkd = true;
    networking.useDHCP = false;

    # Enable network in initrd for iSCSI
    boot.initrd.network = {
      enable = true;
      flushBeforeStage2 = false;
      udhcpc.enable = true;
    };

    boot.initrd.kernelModules = [
      "iscsi_tcp"
      "iscsi_ibft"
      "libiscsi"
      "libiscsi_tcp"
      "scsi_transport_iscsi"
      "sd_mod"
      "mlx5_core"
      "virtio_net"
    ]
    ++ lib.optionals cfg.enableLVM [
      "dm-mod"
      "dm-snapshot"
      "dm-mirror"
    ];

    boot.initrd.systemd.enable = false;
    boot.kernelParams = [ "boot.shell_on_fail" ];

    boot.initrd.extraUtilsCommands = lib.mkMerge [
      ''
        copy_bin_and_libs ${pkgs.openiscsi}/bin/iscsid
        copy_bin_and_libs ${pkgs.openiscsi}/bin/iscsiadm
        copy_bin_and_libs ${pkgs.util-linux}/bin/lsblk

        mkdir -p $out/etc/iscsi
        cp ${pkgs.openiscsi}/etc/iscsi/iscsid.conf $out/etc/iscsi/iscsid.conf

        cp -pv ${pkgs.glibc.out}/lib/libnss_files.so.* $out/lib
      ''
      (lib.mkIf cfg.enableLVM ''
        copy_bin_and_libs ${pkgs.lvm2.bin}/bin/lvm
        copy_bin_and_libs ${pkgs.lvm2.bin}/bin/pvcreate
        copy_bin_and_libs ${pkgs.lvm2.bin}/bin/vgcreate
        copy_bin_and_libs ${pkgs.lvm2.bin}/bin/vgchange
        copy_bin_and_libs ${pkgs.lvm2.bin}/bin/vgscan
        copy_bin_and_libs ${pkgs.lvm2.bin}/bin/lvchange
        copy_bin_and_libs ${pkgs.lvm2.bin}/bin/lvscan
      '')
    ];

    boot.initrd.extraUtilsCommandsTest = ''
      $out/bin/iscsiadm --version
    '';

    boot.initrd.preLVMCommands = ''
      echo 'root:x:0:0:root:/root:/bin/ash' > /etc/passwd
      echo 'passwd: files' > /etc/nsswitch.conf

      mkdir -p /etc/iscsi /run/lock/iscsi /var/lib/iscsi/nodes /var/lib/iscsi/send_targets /var/lib/iscsi/ifaces

      if [ -f /sys/firmware/ibft/initiator/initiator-name ]; then
        cat /sys/firmware/ibft/initiator/initiator-name > /etc/iscsi/initiatorname.iscsi
      else
        echo "InitiatorName=iqn.2024-01.org.nixos:$(hostname)" > /etc/iscsi/initiatorname.iscsi
      fi

      cp $extraUtils/etc/iscsi/iscsid.conf /etc/iscsi/iscsid.conf

      cat >> /etc/iscsi/iscsid.conf <<'ISCSI_TUNE'
      node.conn[0].timeo.noop_out_interval = 5
      node.conn[0].timeo.noop_out_timeout = 5
      node.session.timeo.replacement_timeout = 600
      ISCSI_TUNE

      echo "Starting iSCSI daemon..."
      iscsid --foreground --no-pid-file --debug 8 &
      sleep 2

      echo "Connecting to iSCSI target via iBFT..."
      iscsiadm -m fw --login || {
        echo "iBFT login failed, trying manual discovery..."
        if [ -d /sys/firmware/ibft/target0 ]; then
          TARGET_IP=$(cat /sys/firmware/ibft/target0/ip-addr 2>/dev/null)
          TARGET_PORT=$(cat /sys/firmware/ibft/target0/port 2>/dev/null || echo 3260)
          TARGET_NAME=$(cat /sys/firmware/ibft/target0/target-name 2>/dev/null)
          echo "iBFT target: $TARGET_NAME at $TARGET_IP:$TARGET_PORT"
          if [ -n "$TARGET_IP" ] && [ -n "$TARGET_NAME" ]; then
            iscsiadm -m discovery -t sendtargets -p "$TARGET_IP:$TARGET_PORT" --debug 8
            iscsiadm -m node -T "$TARGET_NAME" -p "$TARGET_IP:$TARGET_PORT" --login
          fi
        fi
      }

      sleep 3
      echo "Block devices after iSCSI login:"
      lsblk || cat /proc/partitions

      pkill -9 iscsid || true
    '';

    services.openiscsi = {
      enable = true;
      name = "iqn.2015-02.oracle.boot";
      extraConfig = ''
        node.conn[0].timeo.noop_out_interval = 30
        node.conn[0].timeo.noop_out_timeout = 300
        node.session.timeo.replacement_timeout = 600
      '';
    };

    services.lvm.enable = cfg.enableLVM;

    boot.initrd.network.ssh = lib.mkIf cfg.initrdSSH.enable {
      enable = true;
      port = 22;
      authorizedKeys = cfg.initrdSSH.authorizedKeys;
      hostKeys = lib.optional (cfg.initrdSSH.hostKeyFile != null) cfg.initrdSSH.hostKeyFile;
    };
  };
}

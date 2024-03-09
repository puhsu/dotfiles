# Personal Server Config
# Installed NixOS manually first, via nixos-infect for example

{config, lib, pkgs, modulesPath, ...}: {
  # dunno if i need this
  imports = [(modulesPath + "/profiles/qemu-guest.nix")];
  boot.loader.grub.device = "/dev/vda";
  boot.initrd.availableKernelModules = [ "ata_piix" "uhci_hcd" "xen_blkfront" "vmw_pvscsi" ];
  boot.initrd.kernelModules = [ "nvme" ];
  fileSystems."/" = { device = "/dev/vda1"; fsType = "ext4"; };

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  networking.firewall = {
    allowedTCPPorts = [ 22 80 443 ];
  };
  
  services.qemuGuest = {
    enable = true;
  };

  systemd.services.qemu-guest-agent.serviceConfig.ExecStart = lib.mkForce "${config.services.qemuGuest.package}/bin/qemu-ga --statedir /var/run";

  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
    };
  };

  # dummy caddy conifg
  services.caddy = {
    enable = true;
    virtualHosts."localhost".extraConfig = ''
      respond "Hello, world!"
    '';
  };

  users.users.irubachev = {
    isNormalUser = true;
    shell = pkgs.fish;
    openssh.authorizedKeys.keys = [
      "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBDrz1C/Ra9cRzg2nnZvc5DthwcHPJzUykBCrWtsAO+vwucOKNMu4ONXV8hThvX6h97voLC8XA8rV9Vf9Y2Nd4yU= irubachev@irubachev-osx"
    ];
  };

  users.users.root.openssh.authorizedKeys.keys =
    config.users.users.irubachev.openssh.authorizedKeys.keys;
}

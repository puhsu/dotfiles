{modulesPath, ...}: {
  imports = [(modulesPath + "/profiles/qemu-guest.nix")];
  boot.loader.grub.device = "/dev/vda";
  boot.initrd.availableKernelModules = [ "ata_piix" "uhci_hcd" "xen_blkfront" "vmw_pvscsi" ];
  boot.initrd.kernelModules = [ "nvme" ];
  fileSystems."/" = { device = "/dev/vda1"; fsType = "ext4"; };

  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
    };
  };

  # dummy caddy conifg
  services.caddy = {
    enabled = true;
    virtualHosts."localhost".extraConfig = ''
      respond "Hello, world!"
    '';
  };

  users.users.root.openssh.authorizedKeys.keys = [
    "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBDrz1C/Ra9cRzg2nnZvc5DthwcHPJzUykBCrWtsAO+vwucOKNMu4ONXV8hThvX6h97voLC8XA8rV9Vf9Y2Nd4yU= irubachev@irubachev-osx"
  ];
}

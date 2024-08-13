{...}: {
  networking.hostName = "astoria"; # Define your hostname.
  networking.enableIPv6 = false;

  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  services.resolved = {
    enable = true;
    domains = ["lan"];
    fallbackDns = ["192.168.0.2"];
    extraConfig = ''
      DNS=192.168.0.2
    '';
  };

  networking.networkmanager = {
    enable = true;
  };

  # This command causes a failure to rebuild
  # https://github.com/NixOS/nixpkgs/issues/180175
  systemd.services.NetworkManager-wait-online.enable = false;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ 22 ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # TODO
  networking.firewall.enable = false;
}

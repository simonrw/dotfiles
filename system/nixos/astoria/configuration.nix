# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./syncthing.nix
      ./yubikey.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  networking.hostName = "astoria"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/London";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_GB.utf8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_GB.UTF-8";
    LC_IDENTIFICATION = "en_GB.UTF-8";
    LC_MEASUREMENT = "en_GB.UTF-8";
    LC_MONETARY = "en_GB.UTF-8";
    LC_NAME = "en_GB.UTF-8";
    LC_NUMERIC = "en_GB.UTF-8";
    LC_PAPER = "en_GB.UTF-8";
    LC_TELEPHONE = "en_GB.UTF-8";
    LC_TIME = "en_GB.UTF-8";
  };

  fonts = {
    fontconfig = {
      antialias = true;
      hinting.enable = true;
      hinting.autohint = true;
    };
    fonts = with pkgs; [
      source-code-pro
      fira-code
    ];
  };

  # Configure keymap in X11
  services.xserver = {
    enable = true;
    videoDrivers = [ "nvidia" ];
    layout = "gb";
    xkbVariant = "";
    xkbOptions = "ctrl:nocaps";
    displayManager = {
      sddm.enable = true;
      defaultSession = "xfce";
      # Disable automatic login for the user.
      autoLogin.enable = false;
    };
    desktopManager.xfce.enable = true;
  };

  # Configure console keymap
  console.keyMap = "uk";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # remote desktop
  services.xrdp.enable = true;
  services.xrdp.defaultWindowManager = "startxfce4";

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  services.gnome.gnome-keyring.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.simon = {
    isNormalUser = true;
    description = "Simon Walker";
    extraGroups = [ "networkmanager" "wheel" "libvirtd" "docker" "podman" ];
    packages = with pkgs; [
    ];
    shell = pkgs.fish;
    home = "/home/simon";
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Configure virtualisation
  virtualisation.libvirtd.enable = true;
  virtualisation.podman = {
    enable = true;
    autoPrune = {
      enable = true;
      flags = [
        "--all"
      ];
    };
  };
  virtualisation.docker = {
    enable = true;
    autoPrune = {
      enable = true;
      flags = [
        "--all"
      ];
    };
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    alacritty
    firefox
    git
    google-chrome
    killall
    kitty
    pavucontrol
    steam
    vim
    wmctrl
    xclip
    xfce.xfce4-systemload-plugin
    xfce.xfce4-cpugraph-plugin
    xfce.xfce4-cpufreq-plugin
    xfce.xfce4-pulseaudio-plugin
  ];

  environment.shells = with pkgs; [
    fish
    zsh
    bashInteractive
  ];

  # enable fish system-wide for completion
  programs.fish.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.passwordAuthentication = false;
  services.openssh.permitRootLogin = "no";

  services.blueman.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ 22 ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # TODO
  networking.firewall.enable = false;

  nix.extraOptions = ''
    # https://jackson.dev/post/nix-reasonable-defaults/
    connect-timeout = 5
    log-lines = 25

    experimental-features = nix-command flakes
    fallback = true
    warn-dirty = false
    auto-optimise-store = true

    # additional settings
    keep-outputs = false
    keep-derivations = true
    trusted-users = simon root
  '';
  nix.settings.trusted-users = [ "root" "simon" ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?

}

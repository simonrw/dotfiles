# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  pkgs,
  lib,
  ...
}: let
  default-locale = "en_GB.UTF-8";
in {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    # ./yubikey.nix
    # ./logitech.nix
    ./wm.nix
    ./networking.nix
    ./update-diff.nix
  ];

  # Bootloader.
  boot.loader.systemd-boot = {
    enable = true;
    configurationLimit = 5;
  };
  boot.loader.efi.canTouchEfiVariables = true;

  # emulated systems
  boot.binfmt.emulatedSystems = [
    "x86_64-linux"
  ];

  # Set your time zone.
  time.timeZone = "Europe/London";

  # Select internationalisation properties.
  i18n.defaultLocale = default-locale;

  i18n.extraLocaleSettings = {
    LC_ADDRESS = default-locale;
    LC_IDENTIFICATION = default-locale;
    LC_MEASUREMENT = default-locale;
    LC_MONETARY = default-locale;
    LC_NAME = default-locale;
    LC_NUMERIC = default-locale;
    LC_PAPER = default-locale;
    LC_TELEPHONE = default-locale;
    LC_TIME = default-locale;
  };

  fonts = {
    packages = with pkgs; [
      source-code-pro
      fira-code
      jetbrains-mono
      inconsolata
      hack-font
    ];
  };

  # Configure keymap in X11
  services.xserver = {
    enable = true;
    layout = "gb";
    xkbVariant = "";
    xkbOptions = "ctrl:nocaps";
    displayManager = {
      # Disable automatic login for the user.
      autoLogin.enable = false;
    };
  };

  # Don't require password for users in `wheel` group for these commands
  # https://github.com/dustinlyons/nixos-config/blob/84f23336a83363f287ebde8d25879cb53f1ec4c8/nixos/default.nix#L256C1-L269C1
  security.sudo = {
    enable = true;
    extraRules = [
      {
        commands = [
          {
            command = "${pkgs.systemd}/bin/reboot";
            options = ["NOPASSWD"];
          }
        ];
        groups = ["wheel"];
      }
    ];
  };

  # Configure console keymap
  console.keyMap = "uk";

  services.gnome.gnome-keyring.enable = true;

  services.tailscale = {
    enable = true;
    extraUpFlags = [
      "--ssh"
    ];
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users = {
    simon = {
      isNormalUser = true;
      description = "Simon Walker";
      extraGroups = ["libvirt" "kvm" "networkmanager" "wheel" "libvirtd" "docker" "podman" "input" "bluetooth" "plugdev" "wireshark"];
      shell = pkgs.fish;
      home = "/home/simon";
      initialPassword = "test.1234";
    };
  };

  # Configure virtualisation
  virtualisation.libvirtd.enable = true;
  virtualisation.podman = {
    enable = true;
    autoPrune = {
      enable = true;
      dates = "monthly";
      flags = [
        "--all"
        "--filter=24h"
      ];
    };
    defaultNetwork.settings.dns_enabled = true;
  };
  virtualisation.docker = {
    enable = true;
    autoPrune = {
      dates = "monthly";
      enable = true;
      flags = [
        "--all"
        "--filter=24h"
      ];
    };
    enableOnBoot = false;
  };

  # configure man pages
  documentation = {
    enable = true;
    dev.enable = true;
    man.enable = true;
  };

  # List packages installed in system profile. To search, run:
  environment.systemPackages = with pkgs; [
    alacritty
    barrier
    chromium
    firefox
    # currently broken
    # heroic
    inkscape
    pavucontrol
    wmctrl
    xclip
    (writeShellScriptBin "xrandr-auto" ''
      xrandr --output Virtual-1 --auto
    '')
    gtkmm3
  ];

  environment.shells = with pkgs; [
    fish
    zsh
    bashInteractive
  ];

  # enable fish system-wide for completion
  programs.fish.enable = true;
  # disable command-not-found in favour of nix-index
  programs.command-not-found.enable = false;

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = false;
    };
  };

  # enable support for monitoring traffic via wireshark
  programs.wireshark = {
    enable = true;
    package = pkgs.wireshark;
  };

  # configure process limits for users
  security.pam.loginLimits = [
    # increase the soft limit of number of open files
    {
      domain = "*";
      type = "soft";
      item = "nofile";
      value = "4096";
    }
  ];

  nix = import ../../../common/nix-settings.nix {inherit pkgs;};

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}

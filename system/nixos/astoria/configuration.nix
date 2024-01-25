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
    ./yubikey.nix
    ./logitech.nix
    ./wm.nix
    ./networking.nix
    ./update-diff.nix
    ./ddccontrol.nix
  ];

  config = {
    # Bootloader.
    boot.loader.systemd-boot = {
      enable = true;
      configurationLimit = 20;
    };
    boot.loader.efi.canTouchEfiVariables = true;
    boot.loader.efi.efiSysMountPoint = "/boot/efi";

    # emulated systems
    boot.binfmt.emulatedSystems = [
      "aarch64-linux"
    ];

    # put /tmp on a tmpfs
    boot.tmp = {
      useTmpfs = true;
    };

    # sysctl
    # up the map count for pypi warehouse
    boot.kernel.sysctl = {
      # https://warehouse.pypa.io/development/getting-started.html#running-the-warehouse-container-and-services
      "vm.max_map_count" = 262144;
    };

    # for osx-kvm
    boot.extraModprobeConfig = ''
      options kvm_intel nested=1
      options kvm_intel emulate_invalid_guest_state=0
      options kvm ignore_msrs=1 report_ignored_msrs=0
    '';

    # enable graphical cpu governer selection
    services.cpupower-gui.enable = true;

    # enable prometheus metrics collecting
    # services.prometheus = {
    #   enable = true;
    #   exporters = {
    #     node = {
    #       enable = true;
    #       enabledCollectors = [ "systemd" ];
    #     };
    #     snmp = {
    #       enable = true;
    #       configurationPath = "${pkgs.prometheus-snmp-exporter.src}/snmp.yml";
    #     };
    #   };
    #   scrapeConfigs = [
    #     {
    #       job_name = "astoria";
    #       static_configs = [{
    #         targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.node.port}" ];
    #       }];
    #     }
    #     {
    #       job_name = "dns";
    #       static_configs = [{
    #         targets = [ "127.0.0.1:9153" ];
    #       }];
    #     }
    #     {
    #       job_name = "router";
    #       static_configs = [{
    #         targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.snmp.port}" ];
    #       }];
    #       metrics_path = "/snmp";
    #       params = { module = [ "if_mib" ]; target = [ "192.168.0.1" ]; };
    #     }
    #   ];
    # };
    # services.grafana = {
    #   enable = true;
    #   settings.server.http_addr = "127.0.0.1";
    # };

    # enable 1password
    programs._1password.enable = true;
    programs._1password-gui = {
      enable = true;
      polkitPolicyOwners = ["simon"];
    };

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
        monaspace
      ];
    };

    # Configure atd
    services.atd = {
      enable = true;
    };

    # Configure keymap in X11
    services.xserver = {
      enable = true;
      videoDrivers = ["nvidia"];
      layout = "gb";
      xkbVariant = "";
      xkbOptions = "ctrl:nocaps";
      displayManager = {
        # Disable automatic login for the user.
        autoLogin.enable = false;
      };
      wacom.enable = true;
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

    # Enable CUPS to print documents.
    services.printing.enable = true;

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
    # settings to add if this is a virtual machine
    virtualisation.vmVariant = {
      virtualisation.qemu.options = ["-vga virtio" "-smp 4" "-m 16384"];
      documentation.enable = lib.mkForce false;
    };

    # configure man pages
    documentation = {
      enable = true;
      dev.enable = true;
      man.enable = true;
    };

    # Configure steam
    programs.steam = {
      enable = true;
      remotePlay.openFirewall = true;
      # https://github.com/NixOS/nixpkgs/issues/236561#issuecomment-1581879353
      package = with pkgs; steam.override {extraPkgs = pkgs: [attr];};
    };

    # List packages installed in system profile. To search, run:
    # $ nix search wget
    environment.systemPackages = with pkgs; [
      alacritty
      barrier
      chromium
      firefox-devedition
      google-chrome
      # currently broken
      # heroic
      inkscape
      pavucontrol
      wmctrl
      xclip
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

    # Some programs need SUID wrappers, can be configured further or are
    # started in user sessions.
    # programs.mtr.enable = true;
    # programs.gnupg.agent = {
    #   enable = true;
    #   enableSSHSupport = true;
    # };

    # List services that you want to enable:

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

    # configure the system for zsa keyboards
    hardware.keyboard.zsa.enable = true;

    nix = import ../../../common/nix-settings.nix {inherit pkgs;};

    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = "22.05"; # Did you read the comment?
  };
}

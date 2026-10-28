# Ubuntu provisioning

Run from the repository root. Provisioning targets the local Ubuntu system; the downloaded desktop packages target Ubuntu 24.04 on amd64.

```sh
mise run ansible -- -e dotfiles_profile=headless
mise run ansible -- -e dotfiles_profile=desktop
mise run ansible -- -e dotfiles_profile=astoria
```

`desktop` is the default. It installs GUI applications, fonts, snaps, Sunshine, x11vnc, and virtualization tools. `astoria` adds managed user groups and shell, plus Wake-on-LAN on `eno1`. Select `astoria` explicitly on the personal desktop; hostnames no longer select behavior.

`headless` installs CLI and development tools, including `emacs-nox`, Claude Code, and unattended upgrades. It skips all 1Password setup, GUI apps, fonts, snaps, remote desktop, virtualization tools, Wake-on-LAN, and account changes.

## Feature overrides

Each flag can be overridden independently of its profile:

```sh
mise run ansible -- -e dotfiles_profile=desktop -e fonts_enabled=false -e sunshine_enabled=false
mise run ansible -- -e dotfiles_profile=headless -e ollama_enabled=true
mise run ansible -- -e dotfiles_profile=headless -e virtualization_enabled=true
```

Defaults live in [vars.yml](vars.yml). `desktop_enabled` controls GUI Emacs, GNOME Tweaks, and wmctrl. These flags default to the value of `desktop_enabled`:

- `fonts_enabled`: JetBrains Mono and Lilex installation.
- `ghostty_enabled`, `helium_enabled`, `zed_enabled`: graphical terminal, browser, and editor.
- `snaps_enabled`: packages in `snaps` and `classic_snaps`.
- `dconf_enabled`: GNOME settings and their Python dependency.
- `onepassword_gui_enabled`: 1Password desktop application.

Other flags default to false:

- `sunshine_enabled`, `x11vnc_enabled`: remote desktop installation and x11vnc user service enablement.
- `virtualization_enabled`: KVM/QEMU tools, with virt-manager when `desktop_enabled` is true.
- `wakeonlan_enabled`: Wake-on-LAN services for `wakeonlan_interfaces`, which defaults to an empty list.
- `user_management_enabled`: configure `dotfiles_user_shell` and `dotfiles_user_groups`.
- `hyprland_enabled`, `sway_enabled`, `ollama_enabled`: optional window managers and local model service.

`claude_code_enabled`, `docker_enabled`, and `unattended_upgrades_enabled` default to true in every profile. `onepassword_cli_enabled` defaults to true; the headless profile disables both 1Password flags. The desktop and astoria profiles also enable Sunshine, x11vnc, and virtualization.

A false flag skips provisioning. It leaves installed packages, repositories, settings, and running services in place. This also applies to Hyprland, Sway, and Ollama, which previously had removal tasks. Uninstall or stop existing software separately when changing an already provisioned system.

## Docker

Every profile installs Docker CE, Buildx, and the Docker Compose plugin using `geerlingguy.docker`, and starts and enables the Docker service. `mise run ansible` installs the pinned role from [requirements.yml](requirements.yml) before running the playbook. Use `-e docker_enabled=false` to skip Docker provisioning.

Docker access requires `sudo` unless the user belongs to the `docker` group. The astoria profile already manages this membership. For other profiles, opt in with `-e '{"docker_users": ["your-username"]}'`. Membership grants root-equivalent access; log out and back in after adding it.

## Custom profiles

Add a YAML file under [profiles](profiles) containing flag values and any settings to override, then select its filename without `.yml` using `dotfiles_profile`. Loading order is defaults, profile, then Ansible extra variables. An unknown profile fails instead of falling back to another configuration.

For machine-specific overrides without a tracked profile, pass an extra-vars file:

```sh
mise run ansible -- -e dotfiles_profile=desktop -e @/path/to/host.yml
```

Profiles control Ansible provisioning. Dotfile linking remains managed by `mise run dotfiles`.

## Preview

```sh
ansible-galaxy role install -r ansible/requirements.yml
ansible-playbook ansible/main.yml -e dotfiles_profile=headless --syntax-check
ansible-playbook ansible/main.yml -e dotfiles_profile=headless --list-tasks
mise run ansible -- -e dotfiles_profile=headless --check --diff
```

`--list-tasks` includes conditional tasks and does not expand dynamic includes. Check mode does not fully simulate downloaded installers or a fresh system. Review the selected profile before provisioning.

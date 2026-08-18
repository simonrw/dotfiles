require_relative("brew_helpers")

# Taps

tap "railwaycat/emacsmacport", trusted: { casks: ["railwaycat/emacsmacport/emacs-mac"] }
tap "simonrw/tap", trusted: true

# Brews

brew "atuin"
brew "autoconf"
brew "automake"
brew "awscli"
brew "bacon"
brew "bash"
brew "bat"
brew "bob"
brew "btop"
brew "cargo-instruments"
brew "cfitsio"
brew "cmake"
brew "colima" if Helpers.docker_emulator == 'colima'
brew "container"
brew "coreutils"
brew "curl"
brew "delta"
brew "dive"
brew "docker-buildx" if Helpers.docker_emulator == 'colima'
brew "docker-compose" if Helpers.docker_emulator == 'colima'
brew "docker-credential-helper" if Helpers.docker_emulator == 'colima'
brew "docker" if Helpers.docker_emulator == 'colima'
brew "dos2unix"
brew "duckdb"
brew "dust"
brew "e2fsprogs"
brew "eza"
brew "fd"
brew "ffmpeg"
brew "fish"
brew "fzf"
brew "gh"
brew "git-absorb"
brew "git-lfs"
brew "git"
brew "gnu-sed"
brew "go"
brew "gopls"
brew "gpg"
brew "graphviz"
brew "grep"
brew "helix"
brew "herdr"
brew "hey"
brew "htop"
brew "hyperfine"
brew "imagemagick"
brew "iperf3"
brew "ispell"
brew "jjui"
brew "jless"
brew "jujutsu"
brew "lazygit"
brew "lima"
brew "lua"
brew "luajit"
brew "make"
brew "mas"
brew "mise"
brew "mkcert"
brew "ncdu"
brew "node"
brew "pandoc"
brew "pi-coding-agent"
brew "pkg-config"
brew "pnpm"
brew "pstree"
brew "python"
brew "qemu"
brew "reattach-to-user-namespace"
brew "ripgrep"
brew "rsync"
brew "slides"
brew "sqlite3"
brew "stow"
brew "teamtype"
brew "tmux"
brew "tokei"
brew "tree-sitter-cli"
brew "universal-ctags"
brew "uv"
brew "viddy"
brew "wakeonlan"
brew "watchexec"
brew "worktrunk"
brew "xcodegen" if not Helpers.is_work
brew "xh"
brew "yq"
brew "yt-dlp"
brew "zsh"


# for localStack

brew "aws-sam-cli"
brew "bitwarden-cli"
brew "libvirt"
brew "snappy"

# Casks

## music

cask "ableton-live-lite" if Helpers.hostname == "mm"
cask "focusrite-control-2" if not Helpers.is_work

## other

cask "1password-cli"
cask "1password"
cask "betterdisplay"
cask "blender" if not Helpers.is_work
cask "chatgpt"
cask "claude"
cask "codex"
cask "docker-desktop" if Helpers.docker_emulator == "docker-desktop"
cask "fluidvoice"
cask "font-jetbrains-mono-nerd-font"
cask "font-lilex"
cask "ghostty"
cask "gimp"
cask "gitbutler"
cask "godot" if not Helpers.is_work
cask "google-chrome" if not Helpers.is_work
cask "google-drive" if Helpers.is_work
cask "hammerspoon"
cask "helium-browser"
cask "iina"
cask "inkscape"
cask "karabiner-elements"
cask "keyboard-cleaner" if Helpers.hostname != "mm"
cask "linear"
cask "mitmproxy"
cask "ngrok"
cask "notion"
cask "obs"
cask "obsidian" if not Helpers.is_work
cask "pocket-casts" if not Helpers.is_work
cask "railwaycat/emacsmacport/emacs-mac"
cask "raycast"
cask "shotcut"
cask "slack"
cask "steam" if not Helpers.is_work
cask "tailscale-app"
cask "tldraw"
cask "utm"
cask "vorssaint"
cask "wacom-tablet" if not Helpers.hostname == "mba"
cask "whatsapp"
cask "zed"
cask "zoom"

# for LocalStack
cask "clockify"
cask "pycharm" if Helpers.is_work

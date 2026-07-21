# helpers
hostname = `hostname -s`.strip
is_work = hostname == 'walker-s'

# configuration
if hostname == 'mm'
  docker_emulator = 'docker-desktop'
else
  docker_emulator = 'docker-desktop'
end

# Taps

tap "railwaycat/emacsmacport", trusted: { casks: ["railwaycat/emacsmacport/emacs-mac"] }
tap "simonrw/tap", trusted: true

# Brews

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
brew "colima" if docker_emulator == 'colima'
brew "container"
brew "coreutils"
brew "curl"
brew "delta"
brew "dive"
brew "docker-buildx" if docker_emulator == 'colima'
brew "docker-compose" if docker_emulator == 'colima'
brew "docker-credential-helper" if docker_emulator == 'colima'
brew "docker" if docker_emulator == 'colima'
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
brew "hunk"
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
brew "python"
brew "qemu"
brew "reattach-to-user-namespace"
brew "ripgrep"
brew "rsync"
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
brew "xcodegen" if not is_work
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

cask "ableton-live-lite" if hostname == "mm"
cask "focusrite-control-2" if hostname == "mm"

## other

cask "1password-cli"
cask "1password"
cask "betterdisplay"
cask "blender" if not is_work
cask "claude-code"
cask "claude"
cask "docker-desktop" if docker_emulator == "docker-desktop"
cask "fluidvoice"
cask "font-jetbrains-mono-nerd-font"
cask "font-lilex"
cask "ghostty"
cask "gimp"
cask "godot" if not is_work
cask "google-chrome" if not is_work
cask "google-drive" if is_work
cask "hammerspoon"
cask "helium-browser"
cask "iina"
cask "inkscape"
cask "karabiner-elements"
cask "keyboard-cleaner" if hostname != "mm"
cask "linear"
cask "mitmproxy"
cask "ngrok"
cask "notion"
cask "obs"
cask "obsidian"
cask "paseo"
cask "railwaycat/emacsmacport/emacs-mac"
cask "raycast"
cask "shotcut"
cask "slack"
cask "steam" if not is_work
cask "t3-code"
cask "tailscale-app"
cask "utm"
cask "visual-studio-code" if not is_work
cask "wacom-tablet" if hostname == "mm"
cask "whatsapp"
cask "zed"
cask "zoom"

# for LocalStack
cask "clockify"
cask "pycharm" if is_work

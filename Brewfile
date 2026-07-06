# configuration
if `hostname`.strip == 'mm'
  docker_emulator = 'docker-desktop'
else
  docker_emulator = 'docker-desktop'
end

# Taps

tap "pulumi/tap"
tap "railwaycat/emacsmacport"

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
brew "python"
brew "qemu"
brew "reattach-to-user-namespace"
brew "ripgrep"
brew "rsync"
brew "sqlite3"
brew "stow"
brew "tmux"
brew "tokei"
brew "tree-sitter-cli"
brew "universal-ctags"
brew "uv"
brew "viddy"
brew "wakeonlan"
brew "watchexec"
brew "worktrunk"
brew "xh"
brew "yq"
brew "yt-dlp"
brew "zsh"

# for localStack

brew "aws-sam-cli"
brew "bitwarden-cli"
brew "libvirt"
brew "pulumi/tap/pulumi"
brew "snappy"

# Casks

## music

cask "ableton-live-lite" if `hostname`.strip == "mm"
cask "focusrite-control-2" if `hostname`.strip == "mm"

## other

cask "1password-cli"
cask "1password"
cask "betterdisplay"
cask "blender"
cask "claude"
cask "codex-app"
cask "codex"
cask "docker-desktop" if docker_emulator == "docker-desktop"
cask "font-jetbrains-mono-nerd-font"
cask "font-lilex"
cask "ghostty"
cask "gimp"
cask "godot"
cask "google-chrome"
cask "hammerspoon"
cask "handy"
cask "iina"
cask "inkscape"
cask "karabiner-elements"
cask "keyboard-cleaner" if `hostname`.strip != "mm"
cask "linear"
cask "mitmproxy"
cask "ngrok"
cask "notion"
cask "obs"
cask "obsidian"
cask "railwaycat/emacsmacport/emacs-mac"
cask "raycast"
cask "shotcut"
cask "slack"
cask "steam"
cask "tailscale-app"
cask "utm"
cask "visual-studio-code"
cask "wacom-tablet" if `hostname`.strip == "mm"
cask "whatsapp"
cask "zed"
cask "zoom"

# for LocalStack
cask "clockify"

# Mac App Store apps
mas "1Password for Safari", id: 1569813296
mas "DaisyDisk", id: 411643860
mas "iMovie", id: 408981434
mas "Keynote", id: 361285480
mas "Pages", id: 361309726
mas "Wipr 2", id: 1662217862
mas "Xcode", id: 497799835

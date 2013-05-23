# Hide the ~/Library folder
chflags hidden ~/Library

# Disable deprecated samba support
sudo sysctl -w net.smb.fs.kern_deprecatePreXPServers=1

# Enable the auto closing of apps
defaults write -g NSDisableAutomaticTermination -bool no

# Enable the local time machine backup system
# Link: http://jeetworks.org/node/107
sudo tmutil enablelocal

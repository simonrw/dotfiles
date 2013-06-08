# Show the ~/Library folder
chflags nohidden ~/Library

# Enable deprecated samba support
sudo sysctl -w net.smb.fs.kern_deprecatePreXPServers=0

# Disable the auto closing of apps
defaults write -g NSDisableAutomaticTermination -bool yes

# Disable the local time machine backup system
# Link: http://jeetworks.org/node/107
sudo tmutil disablelocal

# Enable tap to click on the login screen
sudo defaults write /Library/Preferences/.GlobalPreferences com.apple.mouse.tapBehavior -int 1

SYSCTLFILENAME=/etc/sysctl.conf
sudo echo "kern.maxfiles=10485760" >> ${SYSCTLFILENAME}
sudo echo "kern.maxfilesperproc=1048576" >> ${SYSCTLFILENAME}

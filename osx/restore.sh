# Hide the ~/Library folder
chflags hidden ~/Library

# Disable deprecated samba support
sudo sysctl -w net.smb.fs.kern_deprecatePreXPServers=1

# Enable the auto closing of apps
defaults write -g NSDisableAutomaticTermination -bool no

# Enable the local time machine backup system
# Link: http://jeetworks.org/node/107
sudo tmutil enablelocal

# Disable tap to click on the login screen
sudo defaults write /Library/Preferences/.GlobalPreferences com.apple.mouse.tapBehavior -int 0

# This currently does not work...
SYSCTLFILENAME=/etc/sysctl.conf
if [[ -f ${SYSCTLFILENAME} ]]; then
    sudo cat ${SYSCTLFILENAME} \
        | grep  -Ev 'kern\.maxfiles=' \
        | grep -Ev 'kern\.maxfilesperproc=' \
        | grep -Ev 'kern\.sysv\.shmall' \
        | grep -Ev 'kern\.sysv\.shmmax' > /etc/tmp && mv /etc/tmp ${SYSCTLFILENAME}
fi

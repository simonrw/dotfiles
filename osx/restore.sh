# Hide the ~/Library folder
chflags hidden ~/Library

# Disable deprecated samba support
sudo sysctl -w net.smb.fs.kern_deprecatePreXPServers=1

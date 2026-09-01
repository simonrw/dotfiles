# Use the external drive for cargo builds and go package downloads
set -l __LOCATION /mnt/data

if test -d $__LOCATION; and test -r $__LOCATION; and test -w $__LOCATION
    set -gx OLLAMA_MODELS $__LOCATION/ollama
end

# Use external HDD for cargo builds and go package downloads
set -l __LOCATION /Volumes/External

if test -d $__LOCATION; and test -r $__LOCATION; and test -w $__LOCATION
    set -gx OLLAMA_MODELS $__LOCATION/ollama
    set -gx COLIMA_HOME $__LOCATION/colima
end

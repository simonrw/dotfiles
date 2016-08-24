# Function to determine if the system has the command specified.
#
# Taken from https://stackoverflow.com/a/677212/56711
has_executable() {
    if [[ $# != 1 ]]; then
        echo "has_executable requires one argument" >&2
        return 1
    fi

    command -v $1 >/dev/null 2>/dev/null
}

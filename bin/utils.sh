function nixarch() {
    case "$(uname -s) $(uname -m)" in
        "Darwin arm64")
            echo aarch64-darwin
            ;;
        "Darwin x86_64")
            echo x86_64-darwin
            ;;
        "Linux aarch64")
            echo aarch64-linux
            ;;
        "Linux x86_64")
            echo x86_64-linux
            ;;
        *)
            echo "Unknown"
    esac
}

function get-hosttarget() {
    echo "${HOSTTARGET:-$(hostname -s)}"
}

HOSTTARGET="$(get-hosttarget)"

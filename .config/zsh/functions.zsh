mcd() {
    mkdir -p "$1" && cd "$1"
}

tcd() {
    cd "$(mktemp -d "$TMPDIR/tempXXXX")"
    tnew
}

op-signin() {
    if ! op account list 2>/dev/null | command rg -q my.1password.com; then
        op account add --address my.1password.com --shorthand my
    fi
    eval "$(op signin --account my)"
}

pi-update() {
    mise --cd ~ upgrade npm:@earendil-works/pi-coding-agent
}

ssh-add-all() {
    find -L ~/.ssh -type f -name "id_*" -and -not -name "*.pub" | xargs ssh-add
}

is-dark-theme() {
    if [[ "$OSTYPE" == darwin* ]]; then
        defaults read -g AppleInterfaceStyle >/dev/null 2>&1
        return $?
    fi

    if (( ${+commands[gsettings]} )); then
        local colorscheme theme
        colorscheme=$(gsettings get org.gnome.desktop.interface color-scheme 2>/dev/null)
        [[ "$colorscheme" == *dark* ]] && return 0
        [[ "$colorscheme" == *light* ]] && return 1
        theme=$(gsettings get org.gnome.desktop.interface gtk-theme 2>/dev/null)
        [[ "$theme" == *dark* ]] && return 0
    fi
    return 1
}

nono() {
    local theme=latte
    [[ "$__IS_DARK_THEME" == 1 ]] && theme=mocha
    NONO_THEME=$theme command nono "$@"
}

cargo() {
    local -a locate_args
    [[ "$1" == +* ]] && locate_args+=("$1")
    locate_args+=(locate-project --workspace --message-format plain)

    local i
    for (( i=1; i <= $#; i++ )); do
        case "${argv[i]}" in
            --) break ;;
            --target-dir|--target-dir=*)
                command cargo "$@"
                return $? ;;
            --manifest-path|-m)
                (( i++ ))
                (( i <= $# )) && locate_args+=(--manifest-path "${argv[i]}") ;;
            --manifest-path=*) locate_args+=("${argv[i]}") ;;
        esac
    done

    local manifest
    manifest=$(command cargo "${locate_args[@]}" 2>/dev/null) || {
        command cargo "$@"
        return $?
    }

    local hash=$(printf '%s' "$manifest" | shasum -a 256)
    hash=${hash%% *}
    local -x CARGO_TARGET_DIR="$HOME/.cargo-target/$hash"
    local target="${manifest:h}/target"

    if [[ -e "$target" && ! -L "$target" ]]; then
        printf 'cargo: move or remove %s before using the shared target directory\n' "$target" >&2
        return 1
    fi

    mkdir -p "$CARGO_TARGET_DIR" || return
    if [[ ! -L "$target" || "$(readlink "$target")" != "$CARGO_TARGET_DIR" ]]; then
        ln -sfn "$CARGO_TARGET_DIR" "$target" || return
    fi
    command cargo "$@"
}

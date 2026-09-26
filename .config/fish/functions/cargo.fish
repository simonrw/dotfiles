function cargo --wraps cargo --description 'Use a per-workspace Cargo target directory'
    set -l locate_args
    if string match -q '+*' -- "$argv[1]"
        set -a locate_args "$argv[1]"
    end
    set -a locate_args locate-project --workspace --message-format plain

    set -l i 1
    while test $i -le (count $argv)
        switch "$argv[$i]"
            case --
                break
            case --target-dir '--target-dir=*'
                command cargo $argv
                return $status
            case --manifest-path -m
                set i (math $i + 1)
                if test $i -le (count $argv)
                    set -a locate_args --manifest-path "$argv[$i]"
                end
            case '--manifest-path=*'
                set -a locate_args "$argv[$i]"
        end
        set i (math $i + 1)
    end

    # Pass through commands outside a Cargo project, such as `cargo new`.
    set -l manifest (command cargo $locate_args 2>/dev/null)
    if test $status -ne 0
        command cargo $argv
        return $status
    end

    set -l hash (printf '%s' "$manifest" | shasum -a 256 | string split -f 1 ' ')
    set -lx CARGO_TARGET_DIR "$HOME/.cargo-target/$hash"
    set -l target (path dirname "$manifest")/target

    if test -e "$target"; and not test -L "$target"
        printf 'cargo: move or remove %s before using the shared target directory\n' "$target" >&2
        return 1
    end

    mkdir -p "$CARGO_TARGET_DIR"; or return
    if not test -L "$target"; or test (readlink "$target") != "$CARGO_TARGET_DIR"
        ln -sfn "$CARGO_TARGET_DIR" "$target"; or return
    end

    command cargo $argv
end

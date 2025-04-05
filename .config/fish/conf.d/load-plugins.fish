set -l script_dir (realpath (dirname (status --current-filename)))
set -l plugin_stubs plugin-foreign-env plugin-ssh-agent

for plugin_stub in $plugin_stubs
    set -l plugin_dir (realpath "$script_dir"/../plugins/$plugin_stub)

    if ! test -d $plugin_dir
        echo "No plugin found at $plugin_dir, have you cloned with submodules?" >&2
    end

    # Set paths to import plugin components
    if test -d $plugin_dir/functions
        set fish_function_path $fish_function_path[1] $plugin_dir/functions $fish_function_path[2..-1]
    end

    if test -d $plugin_dir/completions
        set fish_complete_path $fish_complete_path[1] $plugin_dir/completions $fish_complete_path[2..-1]
    end

    # Source initialization code if it exists.
    if test -d $plugin_dir/conf.d
        for f in $plugin_dir/conf.d/*.fish
            source $f
        end
    end

    if test -f $plugin_dir/key_bindings.fish
        source $plugin_dir/key_bindings.fish
    end

    if test -f $plugin_dir/init.fish
        source $plugin_dir/init.fish
    end
end

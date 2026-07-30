complete -c djo -f

# Commands
complete -c djo -n __fish_use_subcommand -a config -d 'Configuration management'
complete -c djo -n __fish_use_subcommand -a copy-ignored -d 'Copy untracked files between workspaces'
complete -c djo -n __fish_use_subcommand -a for-each -d 'Run a command in every workspace'
complete -c djo -n __fish_use_subcommand -a help -d 'Display help'
complete -c djo -n __fish_use_subcommand -a hook -d 'Manually run hooks'
complete -c djo -n __fish_use_subcommand -a list -d 'List all jj workspaces'
complete -c djo -n __fish_use_subcommand -a merge -d 'Squash, rebase, move bookmark, and clean up'
complete -c djo -n __fish_use_subcommand -a prune -d 'Remove merged workspaces'
complete -c djo -n __fish_use_subcommand -a remove -d 'Forget a workspace and delete its directory'
complete -c djo -n __fish_use_subcommand -a run -d 'Run a configured alias command'
complete -c djo -n __fish_use_subcommand -a shell -d 'Shell integration commands'
complete -c djo -n __fish_use_subcommand -a switch -d 'Create or switch to a workspace'
complete -c djo -n __fish_use_subcommand -a update-stale -d 'Update stale workspaces'

# Global flags
complete -c djo -s v -l verbose -d 'Verbose output'

# Workspace name completions
for cmd in switch remove copy-ignored hook
  complete -c djo -n "__fish_seen_subcommand_from $cmd" -a "(djo list --json 2>/dev/null | string match -r '\"name\":\"[^\"]*\"' | string replace -r '\"name\":\"([^\"]*)\"' '$1')"
end

# Bookmark completions for merge
complete -c djo -n '__fish_seen_subcommand_from merge' -a "(jj bookmark list --no-pager -T 'name ++ \"\n\"' 2>/dev/null)"

# Shell subcommands
complete -c djo -n '__fish_seen_subcommand_from shell' -a 'completion init install'

# `shell install <shell> [path]`: force file completion for the rc-file path.
complete -c djo -n '__fish_seen_subcommand_from shell; and __fish_seen_subcommand_from install' -F

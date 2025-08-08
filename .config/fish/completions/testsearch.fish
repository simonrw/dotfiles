# Print an optspec for argparse to handle cmd's options that are independent of any subcommand.
function __fish_testsearch_global_optspecs
	string join \n r/root= n/no-fuzzy-selection h/help V/version
end

function __fish_testsearch_needs_command
	# Figure out if the current invocation already has a command.
	set -l cmd (commandline -opc)
	set -e cmd[1]
	argparse -s (__fish_testsearch_global_optspecs) -- $cmd 2>/dev/null
	or return
	if set -q argv[1]
		# Also print the command, so this can be used to figure out what it is.
		echo $argv[1]
		return 1
	end
	return 0
end

function __fish_testsearch_using_subcommand
	set -l cmd (__fish_testsearch_needs_command)
	test -z "$cmd"
	and return 1
	contains -- $cmd[1] $argv
end

complete -c testsearch -n "__fish_testsearch_needs_command" -s r -l root -d 'Paths to search for tests' -r -F
complete -c testsearch -n "__fish_testsearch_needs_command" -s n -l no-fuzzy-selection -d 'Print results rather than using fuzzy find'
complete -c testsearch -n "__fish_testsearch_needs_command" -s h -l help -d 'Print help'
complete -c testsearch -n "__fish_testsearch_needs_command" -s V -l version -d 'Print version'
complete -c testsearch -n "__fish_testsearch_needs_command" -f -a "search"
complete -c testsearch -n "__fish_testsearch_needs_command" -f -a "rerun" -d 'Rerun a previous test'
complete -c testsearch -n "__fish_testsearch_needs_command" -f -a "repl" -d 'Start interactive REPL mode'
complete -c testsearch -n "__fish_testsearch_needs_command" -f -a "grep" -d 'Search for tests containing specific function calls'
complete -c testsearch -n "__fish_testsearch_needs_command" -f -a "state" -d 'View or manage state'
complete -c testsearch -n "__fish_testsearch_needs_command" -f -a "completion" -d 'Generate shell completions'
complete -c testsearch -n "__fish_testsearch_needs_command" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c testsearch -n "__fish_testsearch_using_subcommand search" -s r -l root -d 'Paths to search for tests' -r -F
complete -c testsearch -n "__fish_testsearch_using_subcommand search" -s n -l no-fuzzy-selection -d 'Print results rather than using fuzzy find'
complete -c testsearch -n "__fish_testsearch_using_subcommand search" -s h -l help -d 'Print help'
complete -c testsearch -n "__fish_testsearch_using_subcommand search" -s V -l version -d 'Print version'
complete -c testsearch -n "__fish_testsearch_using_subcommand rerun" -s l -l last -d 'Automatically pick the most recent test'
complete -c testsearch -n "__fish_testsearch_using_subcommand rerun" -s h -l help -d 'Print help'
complete -c testsearch -n "__fish_testsearch_using_subcommand repl" -s h -l help -d 'Print help'
complete -c testsearch -n "__fish_testsearch_using_subcommand grep" -l run -d 'Command template to execute matching tests (use {} as placeholder for test path)' -r
complete -c testsearch -n "__fish_testsearch_using_subcommand grep" -s r -l root -d 'Paths to search for tests' -r -F
complete -c testsearch -n "__fish_testsearch_using_subcommand grep" -s n -l no-fuzzy-selection -d 'Print results rather than using fuzzy find'
complete -c testsearch -n "__fish_testsearch_using_subcommand grep" -s h -l help -d 'Print help'
complete -c testsearch -n "__fish_testsearch_using_subcommand grep" -s V -l version -d 'Print version'
complete -c testsearch -n "__fish_testsearch_using_subcommand state; and not __fish_seen_subcommand_from clear show help" -s h -l help -d 'Print help'
complete -c testsearch -n "__fish_testsearch_using_subcommand state; and not __fish_seen_subcommand_from clear show help" -f -a "clear" -d 'Clear the state'
complete -c testsearch -n "__fish_testsearch_using_subcommand state; and not __fish_seen_subcommand_from clear show help" -f -a "show" -d 'Show the state contents'
complete -c testsearch -n "__fish_testsearch_using_subcommand state; and not __fish_seen_subcommand_from clear show help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c testsearch -n "__fish_testsearch_using_subcommand state; and __fish_seen_subcommand_from clear" -s a -l all -d 'Clear the state for all directories'
complete -c testsearch -n "__fish_testsearch_using_subcommand state; and __fish_seen_subcommand_from clear" -s h -l help -d 'Print help'
complete -c testsearch -n "__fish_testsearch_using_subcommand state; and __fish_seen_subcommand_from show" -s a -l all -d 'Show the last run test for every directory'
complete -c testsearch -n "__fish_testsearch_using_subcommand state; and __fish_seen_subcommand_from show" -s h -l help -d 'Print help'
complete -c testsearch -n "__fish_testsearch_using_subcommand state; and __fish_seen_subcommand_from help" -f -a "clear" -d 'Clear the state'
complete -c testsearch -n "__fish_testsearch_using_subcommand state; and __fish_seen_subcommand_from help" -f -a "show" -d 'Show the state contents'
complete -c testsearch -n "__fish_testsearch_using_subcommand state; and __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c testsearch -n "__fish_testsearch_using_subcommand completion" -s h -l help -d 'Print help'
complete -c testsearch -n "__fish_testsearch_using_subcommand help; and not __fish_seen_subcommand_from search rerun repl grep state completion help" -f -a "search"
complete -c testsearch -n "__fish_testsearch_using_subcommand help; and not __fish_seen_subcommand_from search rerun repl grep state completion help" -f -a "rerun" -d 'Rerun a previous test'
complete -c testsearch -n "__fish_testsearch_using_subcommand help; and not __fish_seen_subcommand_from search rerun repl grep state completion help" -f -a "repl" -d 'Start interactive REPL mode'
complete -c testsearch -n "__fish_testsearch_using_subcommand help; and not __fish_seen_subcommand_from search rerun repl grep state completion help" -f -a "grep" -d 'Search for tests containing specific function calls'
complete -c testsearch -n "__fish_testsearch_using_subcommand help; and not __fish_seen_subcommand_from search rerun repl grep state completion help" -f -a "state" -d 'View or manage state'
complete -c testsearch -n "__fish_testsearch_using_subcommand help; and not __fish_seen_subcommand_from search rerun repl grep state completion help" -f -a "completion" -d 'Generate shell completions'
complete -c testsearch -n "__fish_testsearch_using_subcommand help; and not __fish_seen_subcommand_from search rerun repl grep state completion help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c testsearch -n "__fish_testsearch_using_subcommand help; and __fish_seen_subcommand_from state" -f -a "clear" -d 'Clear the state'
complete -c testsearch -n "__fish_testsearch_using_subcommand help; and __fish_seen_subcommand_from state" -f -a "show" -d 'Show the state contents'

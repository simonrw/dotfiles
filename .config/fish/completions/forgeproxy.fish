# Print an optspec for argparse to handle cmd's options that are independent of any subcommand.
function __fish_forgeproxy_global_optspecs
	string join \n h/help V/version
end

function __fish_forgeproxy_needs_command
	# Figure out if the current invocation already has a command.
	set -l cmd (commandline -opc)
	set -e cmd[1]
	argparse -s (__fish_forgeproxy_global_optspecs) -- $cmd 2>/dev/null
	or return
	if set -q argv[1]
		# Also print the command, so this can be used to figure out what it is.
		echo $argv[1]
		return 1
	end
	return 0
end

function __fish_forgeproxy_using_subcommand
	set -l cmd (__fish_forgeproxy_needs_command)
	test -z "$cmd"
	and return 1
	contains -- $cmd[1] $argv
end

complete -c forgeproxy -n "__fish_forgeproxy_needs_command" -s h -l help -d 'Print help'
complete -c forgeproxy -n "__fish_forgeproxy_needs_command" -s V -l version -d 'Print version'
complete -c forgeproxy -n "__fish_forgeproxy_needs_command" -f -a "server" -d 'Start the proxy server'
complete -c forgeproxy -n "__fish_forgeproxy_needs_command" -f -a "install" -d 'Install platform-specific daemon configuration'
complete -c forgeproxy -n "__fish_forgeproxy_needs_command" -f -a "start" -d 'Install daemon config, start the service, and configure gh'
complete -c forgeproxy -n "__fish_forgeproxy_needs_command" -f -a "stop" -d 'Stop the service and unconfigure gh'
complete -c forgeproxy -n "__fish_forgeproxy_needs_command" -f -a "restart" -d 'Restart the service (stop then start)'
complete -c forgeproxy -n "__fish_forgeproxy_needs_command" -f -a "uninstall" -d 'Remove daemon configuration and stop the service'
complete -c forgeproxy -n "__fish_forgeproxy_needs_command" -f -a "completion" -d 'Generate shell completions'
complete -c forgeproxy -n "__fish_forgeproxy_needs_command" -f -a "status" -d 'Show proxy health and uptime'
complete -c forgeproxy -n "__fish_forgeproxy_needs_command" -f -a "stats" -d 'Show cache statistics'
complete -c forgeproxy -n "__fish_forgeproxy_needs_command" -f -a "rate-limits" -d 'Show GitHub API rate limit state'
complete -c forgeproxy -n "__fish_forgeproxy_needs_command" -f -a "sync-queue" -d 'Show background sync queue'
complete -c forgeproxy -n "__fish_forgeproxy_needs_command" -f -a "cache-keys" -d 'List all cached keys'
complete -c forgeproxy -n "__fish_forgeproxy_needs_command" -f -a "cache-flush" -d 'Flush the entire cache'
complete -c forgeproxy -n "__fish_forgeproxy_needs_command" -f -a "watch" -d 'Watch the sync queue and stats in a live TUI dashboard'
complete -c forgeproxy -n "__fish_forgeproxy_needs_command" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand server" -l socket -d 'Path to the Unix socket to listen on' -r
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand server" -l admin-port -d 'Port for the admin API (0 to disable)' -r
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand server" -l config -d 'Path to config file' -r
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand server" -l log-level -d 'Log level (trace, debug, info, warn, error)' -r
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand server" -s h -l help -d 'Print help'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand install" -l config -d 'Path to config file (to read socket_path)' -r
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand install" -s h -l help -d 'Print help'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand start" -l config -d 'Path to config file (to read socket_path)' -r
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand start" -s h -l help -d 'Print help'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand stop" -l config -d 'Path to config file (to read socket_path)' -r
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand stop" -s h -l help -d 'Print help'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand restart" -l config -d 'Path to config file (to read socket_path)' -r
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand restart" -l reset -d 'Fully reinstall daemon configuration before starting'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand restart" -s h -l help -d 'Print help'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand uninstall" -l config -d 'Path to config file (to read socket_path)' -r
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand uninstall" -s h -l help -d 'Print help'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand completion" -s h -l help -d 'Print help'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand status" -l admin-port -d 'Admin API port (overrides config file)' -r
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand status" -l config -d 'Path to config file (to discover admin_port)' -r
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand status" -s h -l help -d 'Print help'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand stats" -l admin-port -d 'Admin API port (overrides config file)' -r
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand stats" -l config -d 'Path to config file (to discover admin_port)' -r
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand stats" -s h -l help -d 'Print help'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand rate-limits" -l admin-port -d 'Admin API port (overrides config file)' -r
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand rate-limits" -l config -d 'Path to config file (to discover admin_port)' -r
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand rate-limits" -s h -l help -d 'Print help'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand sync-queue" -l admin-port -d 'Admin API port (overrides config file)' -r
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand sync-queue" -l config -d 'Path to config file (to discover admin_port)' -r
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand sync-queue" -s h -l help -d 'Print help'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand cache-keys" -l admin-port -d 'Admin API port (overrides config file)' -r
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand cache-keys" -l config -d 'Path to config file (to discover admin_port)' -r
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand cache-keys" -s h -l help -d 'Print help'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand cache-flush" -l admin-port -d 'Admin API port (overrides config file)' -r
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand cache-flush" -l config -d 'Path to config file (to discover admin_port)' -r
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand cache-flush" -s h -l help -d 'Print help'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand watch" -l admin-port -d 'Admin API port (overrides config file)' -r
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand watch" -l config -d 'Path to config file (to discover admin_port)' -r
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand watch" -s h -l help -d 'Print help'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand help; and not __fish_seen_subcommand_from server install start stop restart uninstall completion status stats rate-limits sync-queue cache-keys cache-flush watch help" -f -a "server" -d 'Start the proxy server'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand help; and not __fish_seen_subcommand_from server install start stop restart uninstall completion status stats rate-limits sync-queue cache-keys cache-flush watch help" -f -a "install" -d 'Install platform-specific daemon configuration'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand help; and not __fish_seen_subcommand_from server install start stop restart uninstall completion status stats rate-limits sync-queue cache-keys cache-flush watch help" -f -a "start" -d 'Install daemon config, start the service, and configure gh'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand help; and not __fish_seen_subcommand_from server install start stop restart uninstall completion status stats rate-limits sync-queue cache-keys cache-flush watch help" -f -a "stop" -d 'Stop the service and unconfigure gh'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand help; and not __fish_seen_subcommand_from server install start stop restart uninstall completion status stats rate-limits sync-queue cache-keys cache-flush watch help" -f -a "restart" -d 'Restart the service (stop then start)'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand help; and not __fish_seen_subcommand_from server install start stop restart uninstall completion status stats rate-limits sync-queue cache-keys cache-flush watch help" -f -a "uninstall" -d 'Remove daemon configuration and stop the service'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand help; and not __fish_seen_subcommand_from server install start stop restart uninstall completion status stats rate-limits sync-queue cache-keys cache-flush watch help" -f -a "completion" -d 'Generate shell completions'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand help; and not __fish_seen_subcommand_from server install start stop restart uninstall completion status stats rate-limits sync-queue cache-keys cache-flush watch help" -f -a "status" -d 'Show proxy health and uptime'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand help; and not __fish_seen_subcommand_from server install start stop restart uninstall completion status stats rate-limits sync-queue cache-keys cache-flush watch help" -f -a "stats" -d 'Show cache statistics'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand help; and not __fish_seen_subcommand_from server install start stop restart uninstall completion status stats rate-limits sync-queue cache-keys cache-flush watch help" -f -a "rate-limits" -d 'Show GitHub API rate limit state'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand help; and not __fish_seen_subcommand_from server install start stop restart uninstall completion status stats rate-limits sync-queue cache-keys cache-flush watch help" -f -a "sync-queue" -d 'Show background sync queue'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand help; and not __fish_seen_subcommand_from server install start stop restart uninstall completion status stats rate-limits sync-queue cache-keys cache-flush watch help" -f -a "cache-keys" -d 'List all cached keys'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand help; and not __fish_seen_subcommand_from server install start stop restart uninstall completion status stats rate-limits sync-queue cache-keys cache-flush watch help" -f -a "cache-flush" -d 'Flush the entire cache'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand help; and not __fish_seen_subcommand_from server install start stop restart uninstall completion status stats rate-limits sync-queue cache-keys cache-flush watch help" -f -a "watch" -d 'Watch the sync queue and stats in a live TUI dashboard'
complete -c forgeproxy -n "__fish_forgeproxy_using_subcommand help; and not __fish_seen_subcommand_from server install start stop restart uninstall completion status stats rate-limits sync-queue cache-keys cache-flush watch help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'

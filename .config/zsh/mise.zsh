export MISE_SHELL=zsh
export __MISE_ORIG_PATH="$PATH"

local MISE_PATH=/opt/homebrew/opt/mise/bin
if ! echo $OSTYPE | grep -q darwin; then
  # not macos
  MISE_PATH=/usr/bin
fi

mise() {
  local command
  command="${1:-}"
  if [ "$#" = 0 ]; then
    command $MISE_PATH/mise
    return
  fi
  shift

  case "$command" in
  deactivate|shell|sh)
    # if argv doesn't contains -h,--help
    if [[ ! " $@ " =~ " --help " ]] && [[ ! " $@ " =~ " -h " ]]; then
      eval "$(command ${MISE_PATH}/mise "$command" "$@")"
      return $?
    fi
    ;;
  esac
  command ${MISE_PATH}/mise "$command" "$@"
}

_mise_hook() {
  eval "$(${MISE_PATH}/mise hook-env -s zsh)";
}
typeset -ag precmd_functions;
if [[ -z "${precmd_functions[(r)_mise_hook]+1}" ]]; then
  precmd_functions=( _mise_hook ${precmd_functions[@]} )
fi
typeset -ag chpwd_functions;
if [[ -z "${chpwd_functions[(r)_mise_hook]+1}" ]]; then
  chpwd_functions=( _mise_hook ${chpwd_functions[@]} )
fi

_mise_hook
if [ -z "${_mise_cmd_not_found:-}" ]; then
    _mise_cmd_not_found=1
    [ -n "$(declare -f command_not_found_handler)" ] && eval "${$(declare -f command_not_found_handler)/command_not_found_handler/_command_not_found_handler}"

    function command_not_found_handler() {
        if [[ "$1" != "mise" && "$1" != "mise-"* ]] && ${MISE_PATH}/mise hook-not-found -s zsh -- "$1"; then
          _mise_hook
          "$@"
        elif [ -n "$(declare -f _command_not_found_handler)" ]; then
            _command_not_found_handler "$@"
        else
            echo "zsh: command not found: $1" >&2
            return 127
        fi
    }
fi

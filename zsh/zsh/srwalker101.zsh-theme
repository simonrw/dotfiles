function __is_git_dir() {
    if [[ -d ".git" ]]; then
echo "[g]"
    else
echo ""
    fi
}

function __get_load() {
    uptime | awk '{print $(NF-2),$(NF-1),$(NF)}' | tr -d ' '
}

function __get_nr_jobs() {
    jobs | wc -l | sed 's/ //g'
}

function __suspended_count() {
    if [[ -n $(jobs -s) ]]; then
        print '= '
    fi
}

function hr() {
    printf '%*s\n' "${COLUMNS:-$(tput cols)}" '' | tr ' ' -
}

function __git_status() {
  cd "$1"
  
  local ref branch lockflag
  
  (( DRACULA_GIT_NOLOCK )) && lockflag="--no-optional-locks"

  ref=$(=git $lockflag symbolic-ref --quiet HEAD 2>/tmp/git-errors)

  case $? in
    0)   ;;
    128) return ;;
    *)   ref=$(=git $lockflag rev-parse --short HEAD 2>/tmp/git-errors) || return ;;
  esac

  branch=${ref#refs/heads/}
  
  if [[ -n $branch ]]; then
    echo -n "${ZSH_THEME_GIT_PROMPT_PREFIX}${branch}"

    local git_status icon
    git_status="$(LC_ALL=C =git $lockflag status --long 2>&1)"
    
    if [[ "$git_status" =~ 'new file:|deleted:|modified:|renamed:|Untracked files:' ]]; then
      echo -n "$ZSH_THEME_GIT_PROMPT_DIRTY"
    else
      echo -n "$ZSH_THEME_GIT_PROMPT_CLEAN"
    fi

    echo -n "$ZSH_THEME_GIT_PROMPT_SUFFIX"
  fi
}

function __git_callback() {
  __GIT_STATUS="$3"
  zle && zle reset-prompt
  async_stop_worker __git_worker __git_status "$(pwd)"
}

function git_async() {
  async_start_worker __git_worker -n
  async_register_callback __git_worker __git_callback
  async_job __git_worker __git_status "$(pwd)"
}

function precmd() {
    git_async
}

ZSH_THEME_GIT_PROMPT_CLEAN=") %F{green}%B✔ "
ZSH_THEME_GIT_PROMPT_DIRTY=") %F{yellow}%B✗ "
ZSH_THEME_GIT_PROMPT_PREFIX="%F{5}%B("
ZSH_THEME_GIT_PROMPT_SUFFIX="%f%b"

# __prompt_icon="➜"
__prompt_icon="$"
export PROMPT=$'\n%F{4}%~%F{7} ${__GIT_STATUS}\n%F{yellow}$(__suspended_count)%(?.%F{green}${__prompt_icon}.%F{red}${__prompt_icon})%F{reset} '
export RPROMPT=$'%F{yellow}%m%F{reset}'
# vim: ft=zsh

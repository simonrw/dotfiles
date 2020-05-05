if ! has_executable direnv; then
    muted_print "direnv not installed"
else
    eval "$(direnv hook zsh)"
fi

show_virtual_env() {
  if [[ -n "$VIRTUAL_ENV" && -n "$DIRENV_DIR" ]]; then
    echo "($(basename $VIRTUAL_ENV))"
  fi
}
PS1='$(show_virtual_env)'$PS1

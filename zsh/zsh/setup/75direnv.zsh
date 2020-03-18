if ! has_executable direnv; then
    printf "\u001b[30;1mdirenv not installed\u001b[0m\n"
else
    eval "$(direnv hook zsh)"
fi

show_virtual_env() {
  if [[ -n "$VIRTUAL_ENV" && -n "$DIRENV_DIR" ]]; then
    echo "($(basename $VIRTUAL_ENV))"
  fi
}
PS1='$(show_virtual_env)'$PS1

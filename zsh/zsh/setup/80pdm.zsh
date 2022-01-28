if [ -n "$PYTHONPATH" ]; then
    export PYTHONPATH="${HOME}/.local/pipx/venvs/pdm/lib/python3.10/site-packages/pdm/pep582":$PYTHONPATH
else
    export PYTHONPATH="${HOME}/.local/pipx/venvs/pdm/lib/python3.10/site-packages/pdm/pep582"
fi

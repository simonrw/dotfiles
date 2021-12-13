if [ -n "$PYTHONPATH" ]; then
    export PYTHONPATH='/usr/local/opt/pdm/libexec/lib/python3.10/site-packages/pdm/pep582':$PYTHONPATH
else
    export PYTHONPATH='/usr/local/opt/pdm/libexec/lib/python3.10/site-packages/pdm/pep582'
fi

c = get_config()

# Subset of matplotlib rcParams that should be different for the inline backend.
c.InlineBackend.rc = {'figure.figsize': (10, 8)}

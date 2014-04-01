c = get_config()

# Subset of matplotlib rcParams that should be different for the inline backend.
c.InlineBackend.rc = {'font.size': 10, 
'figure.figsize': (10, 8), 
'figure.facecolor': 'white', 
'savefig.dpi': 72, 
'figure.subplot.bottom': 0.125, 
'figure.edgecolor': 'white'}
c.InlineBackend.figure_format = 'svg'

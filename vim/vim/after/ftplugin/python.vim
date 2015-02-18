setlocal textwidth=79

" Text wrapping
setlocal formatoptions+=t

" Python autocompletion !
setlocal omnifunc=pythoncomplete#Complete

" Disable spelling
setlocal nospell

" Set the vim-pipe command
let b:vimpipe_command="python"

" Add the colourcolumn
setlocal colorcolumn=80

" Set some custom expansions, basically snippets but without plugins
iabbrev #! #!/usr/bin/env python
iabbrev coding # -*- coding: utf-8 -*-
iabbrev inumpy import numpy as np
iabbrev iplt import matplotlib.pyplot as plt
iabbrev ifmain if __name__ == '__main__':
iabbrev future from __future__ import division, print_function, absolute_import
iabbrev ipyembed import IPython; IPython.embed(); exit()

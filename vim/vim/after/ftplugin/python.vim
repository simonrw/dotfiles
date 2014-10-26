setlocal textwidth=99

setlocal wrap

" Set the makeprg to flake8 for syntax checking
setlocal makeprg=flake8\ %

" Python autocompletion !
setlocal omnifunc=pythoncomplete#Complete

" Disable spelling
setlocal nospell

" Set the flake8 settings
setlocal makeprg=flake8\ %\ --ignore=E501,E128,E123,E126,E261,E262

" Set the vim-pipe command
let b:vimpipe_command="python"

" Add the colourcolumn
setlocal colorcolumn=100

" Set some custom expansions, basically snippets but without plugins
iabbrev #! #!/usr/bin/env python
iabbrev coding # -*- coding: utf-8 -*-
iabbrev inumpy import numpy as np
iabbrev iplt import matplotlib.pyplot as plt
iabbrev ifmain if __name__ == '__main__':
iabbrev future from __future__ import division, print_function, absolute_import
iabbrev ipyembed import IPython; IPython.embed(); exit()

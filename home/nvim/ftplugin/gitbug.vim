" Vim filetype plugin file
" Language:	gitbug
" License:	Apache-2.0 License
" Original Author:	Simon Walker <s.r.walker101@googlemail.com>
" Last Change:		2020 April 14

if exists("b:did_ftplugin")
    finish
endif

runtime! ftplugin/markdown.vim

let b:did_ftplugin = 1
let s:keepcpo= &cpo
set cpo&vim

setlocal comments=:# commentstring=#\ %s formatoptions-=t formatoptions+=croql

setlocal shiftwidth=2
setlocal softtabstop=2
setlocal wrap

let &cpo = s:keepcpo
unlet s:keepcpo

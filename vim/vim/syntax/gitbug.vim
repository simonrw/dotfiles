" Vim filetype plugin file
" Language:	gitbug
" License:	Apache-2.0 License
" Original Author:	Simon Walker <s.r.walker101@googlemail.com>
" Last Change:		2020 April 14

if exists("b:current_syntax")
  finish
endif

runtime! syntax/markdown.vim
unlet! b:current_syntax

syn keyword	confTodo	contained TODO FIXME XXX
" Avoid matching "text#text", used in /etc/disktab and /etc/gettytab
syn match	confComment	"^#.*" contains=confTodo
syn match	confComment	"\s#.*"ms=s+1 contains=confTodo

hi def link confComment	Comment
hi def link confTodo	Todo

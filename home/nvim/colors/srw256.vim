" Based on

runtime colors/lucius.vim

let g:colors_name = "srw256"

if has("nvim")
    highlight Normal guibg=none
    highlight LineNr guibg=none
    highlight SignColumn guibg=none
    highlight CursorLineNr guifg=#c4bfb9 guibg=none
endif

" configure coc-git
if has("nvim")
    highlight DiffAdd guifg=#58db69 ctermfg=2 guibg=none ctermbg=none
    highlight DiffChange guifg=#e09e2b ctermfg=3 guibg=none ctermbg=none
    highlight DiffDelete guifg=#ff2222 ctermfg=1 guibg=none ctermbg=none
endif
highlight DiffText guibg=#7d1816
highlight diffRemoved guifg=#fd8272
highlight diffAdded guifg=#b4fa73
highlight EndOfBuffer guifg=#242424
highlight Comment guifg=#a3a3a3

" colours for terminal mode
let g:terminal_color_0 = '#616261'
let g:terminal_color_1 = '#fd8272'
let g:terminal_color_2 = '#b4fa73'
let g:terminal_color_3 = '#fefcc3'
let g:terminal_color_4 = '#a5d5fe'
let g:terminal_color_5 = '#fd8ffd'
let g:terminal_color_6 = '#d0d1fe'
let g:terminal_color_7 = '#f1f0f2'
let g:terminal_color_8 = '#8d8e8d'
let g:terminal_color_9 = '#fec4bd'
let g:terminal_color_10 = '#d6fcb9'
let g:terminal_color_11 = '#fefdd5'
let g:terminal_color_12 = '#c1e3fe'
let g:terminal_color_13 = '#fdb1fe'
let g:terminal_color_14 = '#e5e6fe'
let g:terminal_color_15 = '#fefffe'

" lsp
hi LspDiagnosticsVirtualTextWarning guifg=#8c7048
hi LspDiagnosticsVirtualTextError guifg=#b34a37
hi LspDiagnosticsVirtualTextInformation guifg=#00ff00
hi LspDiagnosticsVirtualTextHint guifg=#4976a3
hi link LspDiagnosticsSignWarning LspDiagnosticsVirtualTextWarning
hi link LspDiagnosticsSignError LspDiagnosticsVirtualTextError
hi link LspDiagnosticsSignInformation LspDiagnosticsVirtualTextInformation
hi link LspDiagnosticsSignHint LspDiagnosticsVirtualTextHint

" ale
if has("nvim")
    hi ALEErrorSign guifg=#ff0000 guibg=none
    hi ALEWarningSign guifg=#ff922b guibg=none
endif
hi link ALEVirtualTextError ALEErrorSign
hi link ALEVirtualTextWarning ALEWarningSign

if has("nvim-0.7")
    hi WinSeparator guifg=#888888 guibg=none
endif
highlight ColorColumn ctermbg=243 guibg=#222222

let g:linenr_background = 'none'
execute 'highlight LineNr guibg=' . g:linenr_background
execute 'highlight SignColumn guibg=' . g:linenr_background
highlight TabLine guibg=none
highlight TabLineSel guibg=none
highlight TabLineFill guibg=none
execute 'highlight DiagnosticSignError ctermfg=1 guifg=Red guibg=' . g:linenr_background
execute 'highlight DiagnosticSignHint ctermfg=7 guifg=LightGrey guibg=' . g:linenr_background
execute 'highlight DiagnosticSignInfo ctermfg=4 guifg=LightBlue guibg=' . g:linenr_background
execute 'highlight DiagnosticSignWarn ctermfg=3 guifg=Orange guibg=' . g:linenr_background
highlight DiagnosticUnderlineHint guifg=Grey guisp=Grey

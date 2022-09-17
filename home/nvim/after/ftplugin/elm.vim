let g:elm_format_autosave = 1
set shiftwidth=2
set tabstop=2
set expandtab

let test#elm#elmtest#executable = 'npx elm-test'
let b:ale_fixers = ["elm-format"]

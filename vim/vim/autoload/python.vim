function! python#run_mypy(...)
    let l:cmd = 'mypy ' . join(a:000)
    silent cexpr system(l:cmd)
    silent copen
endfunction

function! python#run_flake8(...)
    let l:cmd = 'flake8 ' . join(a:000)
    silent cexpr system(l:cmd)
    silent copen
endfunction


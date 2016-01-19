" Enable c++11 formatting
setlocal syntax=cpp11

" Only write when necessary
nnoremap :w :up
nnoremap :wq :wq
nnoremap :wa :wa
nnoremap :wqa :wqa

map <silent> <leader>y mm:0,$!clang-format<cr>'m

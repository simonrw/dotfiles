" Enable c++11 formatting
setlocal syntax=cpp11

" Only write when necessary
nnoremap :w :up

map <silent> <leader>y :0,$!clang-format<cr>

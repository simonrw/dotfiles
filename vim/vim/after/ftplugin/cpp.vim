" Enable c++11 formatting
setlocal syntax=cpp11

map <silent> <leader>y :0,$!clang-format<cr>

" Enable c++11 formatting
setlocal syntax=cpp11

map <silent> <leader>y mm:0,$!clang-format<cr>'m

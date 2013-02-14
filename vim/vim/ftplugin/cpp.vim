" Enable c++11 formatting
setlocal syntax=cpp11
"
" Strip trailing whitespace from certain filetypes
au BufWritePre <buffer> :%s/\s\+$//e

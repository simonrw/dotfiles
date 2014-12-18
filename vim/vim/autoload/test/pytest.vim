function! test#pytest#test_file(file) abort
  return fnamemodify(a:file, ':t') =~# '^test_.*\.py$'
endfunction

function! test#pytest#build_position(type, position) abort
  if a:type == 'nearest'
    let name = test#pytest#nearest_test(a:position)
    if !empty(name)
      return [a:position['file'].':'.name]
    else
      return [a:position['file']]
    endif
  elseif a:type == 'file'
    return [a:position['file']]
  else
    return []
  endif
endfunction

function! test#pytest#build_args(args) abort
  return ['--exitfirst'] + a:args
endfunction

function! test#pytest#executable() abort
  return 'py.test'
endfunction

function! test#pytest#nearest_test(position) abort
  let method_regex = '\v^\s*def \zstest_\w+'
  let class_regex  = '\v^\s*class \zs\S+\ze:'

  for line in reverse(getbufline(a:position['file'], 1, a:position['line']))
    let method_match = matchstr(line, method_regex)
    let class_match  = matchstr(line, class_regex)

    if !empty(method_match)
      let method_name = method_match
    elseif !empty(class_match)
      let class_name = class_match
      break
    endif
  endfor

  if exists('method_name') && exists('class_name')
    return join([class_name, method_name], '.')
  elseif exists('method_name')
    return method_name
  elseif exists('class_name')
    return class_name
  endif
endfunction

if test -f {$HOME}/.asdf/asdf.fish
    source {$HOME}/.asdf/completions/asdf.fish
else if test -f (brew --prefix asdf)/libexec/asdf.fish
# TODO
end

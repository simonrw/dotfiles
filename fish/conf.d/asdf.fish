if test -f {$HOME}/.asdf/asdf.fish
    source {$HOME}/.asdf/asdf.fish
else if test -f (brew --prefix asdf)/libexec/asdf.fish
    source (brew --prefix asdf)/libexec/asdf.fish
else
    echo "asdf not installed"
end

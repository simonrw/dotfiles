function op-signin --description 'Ensure 1Password account exists and sign in'
    if not op account list 2>/dev/null | grep -q my.1password.com
        op account add --address my.1password.com --shorthand my
    end
    eval (op signin --account my)
end

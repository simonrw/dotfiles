function mcd -a newpath
    if test (count $argv) -ne 1
        echo "No argument given"
        return 1
    end

    mkdir -p "$newpath"
    cd "$newpath"
end

function wakeastoria
    wakeonlan -i 192.168.0.255 -p 7 40:B0:76:DE:79:B3 >/dev/null
    set ip_address "(nslookup astoria | grep Address | grep 192 | awk '{print $2}')"
    if test -z "$ip_address"
        ping -o 192.168.0.10 >/dev/null
    else
        ping -o "$ip_address" >/dev/null
    end
end

function poetry-env-rm -d "Remove current poetry environment"
    poetry env remove (poetry env info --path)/bin/python
end

function gl
    if test (count $argv) -gt 0
        set searchdir $argv[1]
        vim -c "lua require('telescope.builtin').live_grep({ search_dirs = { '$cwd' } })"
    else
        vim -c "lua require('telescope.builtin').live_grep()"
    end
end

function delete-stack -a stackname
    if test -z "$stackname"
        echo "no stack name supplied"
        return 1
    end

    aws cloudformation delete-stack --stack-name "$stackname"
    if test $status -ne 0
        return 1
    end

    aws cloudformation wait stack-delete-complete --stack-name "$stack_name"
end

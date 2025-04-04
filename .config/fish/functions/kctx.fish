function kctx --description="Choose a kubeconfig context"
    set -l global_configs (fd -e yaml -e yml -0 . ~/.kubeconfigs)
    set -l local_configs (timeout 1 fd -e yaml -e yml -0 .)
    set -l chosen (printf "$global_configs\0$local_configs" | fzf --read0 --exit-0)
    if test -z $chosen
        echo "No file chosen"
    end

    echo "Setting KUBECONFIG=$chosen"
    set -gx KUBECONFIG $chosen
end

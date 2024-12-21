set -l global_configs (fd -e yaml -e yml . ~/.kubeconfigs)
set -l local_configs (fd -e yaml -e yml)
set -l chosen (echo $global_configs $local_configs | fzf --exit-0)
if test -z $chosen
    echo "No file chosen"
end

echo "Setting KUBECONFIG=$chosen"
set -g KUBECONFIG $chosen

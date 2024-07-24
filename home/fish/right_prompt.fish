set -l fish_color_status_rprompt (set_color '#6e738d')
set -l normal (set_color normal)

echo -n {$fish_color_status_rprompt} $FOO ms{$normal}


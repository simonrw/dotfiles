complete -c flamegraph -s p -l pid -d 'Profile a running process by pid' -r
complete -c flamegraph -l completions -d 'Generate shell completions for the given shell' -r
complete -c flamegraph -s o -l output -d 'Output file' -r
complete -c flamegraph -s F -l freq -d 'Sampling frequency' -r
complete -c flamegraph -s c -l cmd -d 'Custom command for invoking perf/dtrace' -r
complete -c flamegraph -l notes -d 'Set embedded notes in SVG' -r
complete -c flamegraph -l min-width -d 'Omit functions smaller than <FLOAT> pixels' -r
complete -c flamegraph -l image-width -d 'Image width in pixels' -r
complete -c flamegraph -l palette -d 'Color palette' -r -f -a "{hot	,mem	,io	,red	,green	,blue	,aqua	,yellow	,purple	,orange	,wakeup	,java	,perl	,js	,rust	}"
complete -c flamegraph -l perfdata -r
complete -c flamegraph -s h -l help -d 'Print help information'
complete -c flamegraph -s V -l version -d 'Print version information'
complete -c flamegraph -s v -l verbose -d 'Print extra output to help debug problems'
complete -c flamegraph -l open -d 'Open the output .svg file with default program'
complete -c flamegraph -l root -d 'Run with root privileges (using `sudo`)'
complete -c flamegraph -l deterministic -d 'Colors are selected such that the color of a function does not change between runs'
complete -c flamegraph -s i -l inverted -d 'Plot the flame graph up-side-down'
complete -c flamegraph -l reverse -d 'Generate stack-reversed flame graph'
complete -c flamegraph -l flamechart -d 'Produce a flame chart (sort by time, do not merge stacks)'
complete -c flamegraph -l no-inline -d 'Disable inlining for perf script because of performance issues'

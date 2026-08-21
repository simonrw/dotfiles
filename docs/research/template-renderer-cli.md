# General-purpose template renderer CLIs

Research date: 2026-08-21

## Recommendation

For a small renderer that consumes an arbitrary template and YAML context file, start with `minijinja-cli`. It has the cleanest interface, native YAML support, opt-in environment exposure, familiar Jinja syntax, prebuilt binaries for macOS, Linux, and Windows, and a switch to disable template includes.[1]

Choose `gomplate` if Go templates fit the existing dotfiles better or if directory rendering and richer data sources matter. It loads YAML directly as the root context, exposes the process environment, supports strict missing-key handling, and ships as one binary.[2][3][4]

Choose `chezmoi execute-template` only if adopting chezmoi as the dotfiles data model is desirable. It is the only shortlisted tool that natively supplies reliable OS and hostname values. It also loads user data from `.chezmoidata.yaml`, but it is not a neutral `template + arbitrary context file` CLI.[5][6][7]

## Comparison

| Tool | Template and context | Environment and machine facts | Footprint and portability | Main tradeoff |
| --- | --- | --- | --- | --- |
| `minijinja-cli` | `minijinja-cli template.j2 context.yaml`; supports YAML, JSON/JSON5, TOML, CBOR, INI, query strings, stdin, and inline values.[1] | `--env` adds the process environment as `ENV`. It does not document built-in OS or hostname values, so pass names such as `DOTFILES_OS` and `DOTFILES_HOSTNAME` explicitly.[1] | Rust executable with first-party prebuilt archives for Apple Silicon and Intel macOS, Windows x86/ARM, and GNU/musl Linux across several architectures. `cargo install` and Homebrew are also supported.[1] | Best neutral renderer, but machine facts need a small launcher or explicit values. |
| `gomplate` | `gomplate -f template.tmpl -c .=context.yaml`; `--context` loads the context eagerly and `.` replaces the root context. Supports individual files, stdin, multiple files, and recursive input/output directories.[2][3] | All process environment variables are available through `env.Env` or `env.Getenv`. It does not document native OS or hostname globals, so inject explicit environment values rather than depending on platform-specific variables such as `OSTYPE` or `HOSTNAME`.[2][4] | Single Go binary; official installation includes Homebrew, mise, MacPorts, Chocolatey, Alpine, Docker, direct binary download, and `go install`.[8] | Excellent data and batch features, but a broad function/data-source surface and Go template syntax add complexity. |
| `chezmoi execute-template` | Renders literal arguments, files with `--file`, or stdin. User context comes from `.chezmoidata.{json,jsonc,toml,yaml}` and configuration data, not a context-file positional argument.[5][6] | Built-ins include `.chezmoi.os`, `.chezmoi.arch`, `.chezmoi.hostname`, `.chezmoi.fqdnHostname`, user/home/path values, Linux OS release data, and Windows version data.[6][7] | One cross-platform Go binary, but it brings a complete dotfiles manager rather than only a renderer.[5][6] | Best machine-aware dotfiles solution, but not a general stateless renderer. |

All three projects show repository activity in August 2026, so there is no obvious maintenance red flag at the time of research.[9][10][11]

## Suggested usage

### Minimal and explicit: `minijinja-cli`

Keep the checked-in YAML deterministic and expose only machine values that templates need:

```sh
DOTFILES_OS="$(uname -s | tr '[:upper:]' '[:lower:]')" \
DOTFILES_HOSTNAME="$(hostname -s)" \
minijinja-cli --env template.j2 context.yaml
```

```jinja2
user = {{ user.name }}
os = {{ ENV.DOTFILES_OS }}
hostname = {{ ENV.DOTFILES_HOSTNAME }}
```

This is preferable to exposing every environment variable if rendered output could accidentally contain secrets. Instead, a launcher can build a small YAML object or start the command with `env -i` plus an allowlist.

### Go templates and directories: `gomplate`

```sh
DOTFILES_OS="$(uname -s | tr '[:upper:]' '[:lower:]')" \
DOTFILES_HOSTNAME="$(hostname -s)" \
gomplate -f template.tmpl -c .=context.yaml
```

```gotemplate
user = {{ .user.name }}
os = {{ env.Getenv "DOTFILES_OS" }}
hostname = {{ env.Getenv "DOTFILES_HOSTNAME" }}
```

`env.Getenv` remains available as a function when the root context is replaced by `-c .=context.yaml`.[2][4]

### Native dotfiles model: chezmoi

Put ordinary values in `.chezmoidata.yaml`, then render with:

```sh
chezmoi execute-template --file template.tmpl
```

```gotemplate
user = {{ .user.name }}
os = {{ .chezmoi.os }}
hostname = {{ .chezmoi.hostname }}
```

## Security and determinism

- Treat templates as code. `gomplate` can read files, environment values, local or remote data sources, and can expose external commands through configured plugins.[2][4] chezmoi has a similarly broad dotfiles-oriented function set. Use both only with trusted templates.
- `minijinja-cli` exposes environment variables only when `--env` is passed and can disable includes with `--no-include`, giving it the smallest obvious capability surface of the shortlist.[1]
- OS and hostname are machine facts, not guaranteed environment variables. `OSTYPE` is shell-specific and `HOSTNAME` may be absent or formatted differently. Use native chezmoi variables or normalize `uname` and `hostname` once in a launcher.
- Prefer an allowlisted `machine` object over exposing the entire process environment. This avoids secret leakage and makes tests reproducible.

## Other tools considered

`jinja2-cli` is viable when Python is already guaranteed: it accepts a template and data file, with YAML available through an optional extra, and supports custom Python filters.[12] It is less self-contained than `minijinja-cli` and does not materially improve machine-context handling.

`ytt` is a strong structural YAML processor with Starlark, schemas, overlays, sandboxing, YAML data-value files, and prefixed environment ingestion.[13][14] It is designed to produce YAML rather than arbitrary text, so it is a poor default for mixed dotfiles.

Simple `envsubst` or Mustache binaries are not equivalent: they commonly handle environment variables but do not provide the requested combination of a structured YAML context, general conditionals/iteration, and dependable machine facts.

## Sources

1. [MiniJinja CLI README](https://github.com/mitsuhiko/minijinja/blob/main/minijinja-cli/README.md)
2. [gomplate usage and command-line arguments](https://github.com/hairyhenderson/gomplate/blob/main/docs/content/usage.md)
3. [gomplate README and YAML data-source examples](https://github.com/hairyhenderson/gomplate/blob/main/README.md)
4. [gomplate environment functions](https://github.com/hairyhenderson/gomplate/blob/main/docs/content/functions/env.md)
5. [chezmoi `execute-template` reference](https://github.com/twpayne/chezmoi/blob/master/assets/chezmoi.io/docs/reference/commands/execute-template.md)
6. [chezmoi templating guide and template-data precedence](https://github.com/twpayne/chezmoi/blob/master/assets/chezmoi.io/docs/user-guide/templating.md)
7. [chezmoi automatically populated template variables](https://github.com/twpayne/chezmoi/blob/master/assets/chezmoi.io/docs/reference/templates/variables.md)
8. [gomplate installation documentation](https://github.com/hairyhenderson/gomplate/blob/main/docs/content/installing.md)
9. [Latest gomplate repository commit observed](https://github.com/hairyhenderson/gomplate/commit/2fb505af4a6fdd377ab7e401a726e710af8ffb8d)
10. [Latest MiniJinja repository commit observed](https://github.com/mitsuhiko/minijinja/commit/eb17be5752d267eb446465a23a90e3818a456c6c)
11. [Latest chezmoi repository commit observed](https://github.com/twpayne/chezmoi/commit/38b49b6c70daf0071be98b2ec01b2ab08498e011)
12. [`jinja2-cli` README](https://github.com/mattrobenolt/jinja2-cli/blob/main/README.md)
13. [`ytt` README](https://github.com/carvel-dev/ytt/blob/develop/README.md)
14. [`ytt` data-value flags implementation](https://github.com/carvel-dev/ytt/blob/develop/pkg/cmd/template/data_values_flags.go)

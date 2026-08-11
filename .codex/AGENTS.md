# Writing

* When writing markdown, do not hard wrap text.
* Never use em-dashes: "—", always use ascii dashes: "-"
* Be concise when responding. Use punctuation like bullet lists to convey information when relevant.

# Tooling

* I use ripgrep (rg) not grep
* I use 1Password and the `op` CLI for credential management, but NEVER try to access my credentials using this tool. You can look up account information but don't try to fetch credentials

# Rust

* Do not assume the `target` dir for built outputs. I have a custom path set with `$CARGO_TARGET_DIR` so debug builds are under `$CARGO_TARGET_DIR/debug`, documentation is under `$CARGO_TARGET_DIR/docs` etc.
* Do not assume `~/.cargo` for the cargo registry files. I have a custom path set with `$CARGO_HOME`.
* I use `mise` (https://mise.jdx.dev/) for managing my project tools, so if a tool does not exist in your shell environment, consider using `mise exec -- <command>`

# Notes

When asked to construct a report or write a large document, add a sub-page in Notion to my personal notebook: https://app.notion.com/p/localstack/3b9fc2a234318041a596e6cfff84192a?v=3b9fc2a23431806ba747000cfb6f590d&source=copy_link. Add the content of the note in the body of the page, not in a property. Add appropriate tags that classify the content type.

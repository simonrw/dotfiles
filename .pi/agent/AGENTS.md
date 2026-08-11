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

If I ask you to "summarise in my notebook ", or "make a note " or ask you to do research, or draft a message or blog post, add an entry to my notion notebook database: https://app.notion.com/p/36957aa64add80f0a07deb9165687324?v=36957aa64add8069860a000c63180716&source=copy_link. Add the content of the note in the body of the page, not in a property. Add appropriate tags that classify the content type

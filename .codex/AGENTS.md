# Approaches

I love to focus on simplification, building complex things as simply as possible. I love to reduce complexity.

# Writing

* When writing markdown, do not hard wrap text.
* Never use em-dashes: "—", always use ascii dashes: "-"
* Be concise when responding. Use punctuation like bullet lists to convey information when relevant.
* When writing comments or docstrings in source code, don't explain why the solution is a good one.
    * if writing a comment, explain _why_ the code is needed, not what it does
    * if writing a docstring, explain what the documented object does, not why it is a good design

For example, this is a bad commment:

```go
// Resolve reads the telemetry settings using the supplied lookup, which matches
// the signature of os.LookupEnv. The lookup is a parameter rather than a direct
// os call so the package never touches the process environment itself and tests
// need not mutate it.
```

The second sentence is not needed - it explains why the design decision was a good one. Instead only the first sentence should be written.

# Tooling

* I use ripgrep (rg) not grep
* I use 1Password and the `op` CLI for credential management, but NEVER try to access my credentials using this tool. You can look up account information but don't try to fetch credentials
* You may find errors with commit signing. Read ~/.ssh/agent.fish and extract the ssh agent environment variables and try again.
* I use `mise` (https://mise.jdx.dev/) for managing my project tools, so if a tool does not exist in your shell environment, consider using `mise exec -- <command>`

# Writing code

* When writing code, break logical sections up with newlines to add visual separation and grouping, for example

```python
class Foo:
    """
    Class Foo wraps two separate concepts, `bar` and `baz`.
    """
    def bar(self) -> int:
        return 10

    def baz(self, value: int): ...

# bad
foo = Foo(something)
value =foo.bar()
foo.baz(value * 2)

# good
foo = Foo(something)
value = foo.bar()

foo.baz(value * 2)
```

This is particularly important in tests, separating assertions of different concepts.

# Rust

* Do not assume the `target` dir for built outputs. I have a custom path set with `$CARGO_TARGET_DIR` so debug builds are under `$CARGO_TARGET_DIR/debug`, documentation is under `$CARGO_TARGET_DIR/docs` etc.
* Do not assume `~/.cargo` for the cargo registry files. I have a custom path set with `$CARGO_HOME`.

# Python

* Prefer high level abstractions from the Python standard library, for example prefer operating on python files rather than file descriptors
* Do not introduce cyclic dependencies unless as a last resort
* Prefer global imports over local imports unless resolving a cyclic dependency as a last resort
* Prefer lists over tuples. Only use tuples for fewer than 5 elements where the tuple is literally constructed, e.g. `(a, b, c)` rather than `tuple(a, b, c)` or `tuple(item for item in ...)`. In general prefer native syntax types like lists and dicts (or literally constructed tuples) over built in types that require a function call like tuples.

# Subagent routing

When running inside T3 Code, use T3-owned subagents exclusively.
Recognize T3 from its injected orchestration instructions or T3 runtime
environment, not merely whether its tools are currently visible.

Discover providers and models with `orchestrator_capabilities`, then
use `delegate_task`, including for same-provider tasks. If T3 delegation
is unavailable, report the blocker rather than falling back to native
subagents. Create top-level T3 threads only when explicitly requested.

Outside T3 Code, use the harness's native subagent tools.

Include this routing requirement in delegated task prompts so nested
delegation follows the same rule. This determines how to delegate,
not whether delegation is necessary or authorized.

# Notes

When asked to construct a report or write a large document, add a sub-page in Notion to my personal notebook: https://app.notion.com/p/localstack/3b9fc2a234318041a596e6cfff84192a?v=3b9fc2a23431806ba747000cfb6f590d&source=copy_link. Add the content of the note in the body of the page, not in a property. Add appropriate tags that classify the content type.

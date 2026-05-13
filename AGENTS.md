# Project Guidelines

## GitHub Workflows and Actions

- Use `main` as the default branch name, never `master`
- Always use hash-pinned action versions with a version comment: `uses: actions/checkout@<sha> # v1.2.3`
- Always use the latest stable versions of actions
- Name steps with verb phrases (imperative mood, like Git commits):
  - "Check out" not "Checkout"
  - "Set up Haskell" not "Setup Haskell"
  - "Build project" not "Building"
- Prefer YAML dash syntax for arrays over JSON bracket syntax:

  ```yaml
  # Good
  branches:
    - main

  # Avoid
  branches: [main]
  ```

## Coding Style

- Use descriptive variables
  - abbreviations are only fine if very common like `cmd` for `command`
  - avoid abbreviations for context specific values, like `fbq: FooBarQux`
- Avoid redundant comments, examples are:
  - repeat one line implementations in words
  - commenting a section `Foo configuration` in a configuration file starting with `foo:`
- Use semantic HTML, and avoid `div` and `span` elements when possible
- Prefer `concat` over `++`, even for just two values
- Prefer existing combinators to pattern matching
- Don't inline functions with `let` expressions or pattern matching, extract in variables/functions instead
  - In particular, in DOM code, keep DOM as declarative as possible, and extract all computations into variables/functions.
  - Never write `text (if … then … else …)` inline — extract the string to a named binding first.
  - Never write `disabled (expr1 || expr2)` inline — extract as `isDisabled`.
  - CSS class strings computed from conditionals must be named bindings, not inline `if` inside `class (…)`.
- Never write nested `let … in let … in` blocks — merge all bindings into one `let` block.
- Avoid matching a tuple just to short-circuit, then re-matching each field individually inside the branch. Match each field once in `let` bindings and guard with a derived boolean.
- Extract repeated logic into a shared utility when the repetition represents a genuine domain concept (e.g. "active teams sorted by number", "team display name with fallback"). Do not extract just because code is identical — a one-line view helper used twice may not be worth a shared function.

## Tooling

- Generated code should be version-controlled
- `.env` files should be version-controlled

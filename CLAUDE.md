# CLAUDE.md

## R Coding Conventions

### Package namespacing
Use explicit `pkg::fun()` for every non-base-R function call.
Do **not** use `library()` or `require()` — explicit namespacing makes
dependencies self-documenting and avoids namespace conflicts.

Base R packages are exempt (`base`, `stats`, `utils`, `methods`,
`grDevices`, `graphics`). Examples of exempt functions: `mean()`, `sum()`,
`factor()`, `as.character()`, `cor()`, `as.formula()`, `ifelse()`,
`rowSums()`, `cbind()`, `lapply()`, `type.convert()`, `saveRDS()`,
`readRDS()`, `dir.create()`, `message()`, `cat()`, `sprintf()`.

### Pipe
Use the native R pipe `|>` (R ≥ 4.1). Do **not** use `%>%` from magrittr.

### Style (lintr)
Enforced via `.lintr`. Key rules:
- 2-space indentation
- Line length ≤ 100 characters
- Assignment with `<-`, not `=`
- `TRUE` / `FALSE`, not `T` / `F`
- Spaces around all binary operators and after commas
- No trailing whitespace
- `snake_case` for all object names

Run `lintr::lint_dir("R/")` to check before committing.

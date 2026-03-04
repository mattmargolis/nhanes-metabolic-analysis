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

## Pipeline conventions

- Scripts are numbered `01_` through `06_` and must run in order from the project root.
- `04_metabolic_conditions.R` loads `output/nhanes_merged.rds` (all ages), filters to adults
  ≥18 with valid BMI, and saves **two** outputs:
  - `output/nhanes_metabolic_all.rds` — all adults, used by `06_exercise_analysis.R`
  - `output/nhanes_metabolic_flagged.rds` — healthy-BMI subset, used by `05_analysis.R`
- The adjusted survey weight `wt_adj = wt_full / 2` is added in `04_metabolic_conditions.R`
  and carries through to both downstream scripts.
- `survey::svydesign()` always uses `nest = TRUE` with `id = ~sdmvpsu`,
  `strata = ~sdmvstra`, `weights = ~wt_adj`.
- Subgroup analyses use `subset()` on an existing `svydesign` object (not a new one).

## Cross-cycle variable gotchas

| Variable | Cycle J (2017-18) | Cycle L (2021-22) |
|----------|-------------------|-------------------|
| Blood pressure | BPX_J (auscultatory: BPXSY*/BPXDI*) | BPXO_L (oscillometric: BPXOSY*/BPXODI*) |
| Triglycerides | LBXTR | LBXTLG |
| HTN meds | BPQ050A | BPQ150 |
| Physical activity | PAQ_J | PAQ_L (same variable names) |

## nhanesA quirks

- `nhanesA::nhanes()` may return pre-translated factor labels ("Male", "Yes") in one
  cycle and numeric codes (1, 2) in another. Always use `case_when` handling both forms.
- `bind_cycles()` in `02_clean_merge.R` coerces to character before `bind_rows()` and
  restores types with `type.convert(as.is = TRUE)`.
- For PAQ variables, treat "No" (code 2 / "No") as 0 activity minutes; use
  `if_else(yes_flag == 1L, days * min, 0)` to compute domain minutes.

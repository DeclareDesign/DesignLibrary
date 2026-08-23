# ResearchDesigns audit report

Summary: **60/68** designs OK, **8** failed.

Issue types: `missing_packages`, `yaml_extra_params`, `param_discovery`, `load_error`, `missing_object`, `diagnose_failed`, `disabled`, `other`.

Soft notes (do not fail the audit):
- `no YAML tip`: redesignable param has no tip string in YAML (optional).
- `assigned before design but not redesignable`: top-level `name <- ...` is used by the design but `redesign()` cannot change it (often a fixed vector/data object).
Design steps (`declare_*` pieces) are not parameters. Functions assigned before `design <-` are R-only parameters.
Parked designs (`functional: false`) are listed under Disabled and do not count as failures.
Plain-text listing (`audit_report.txt`) puts FAIL/SKIP first, then OK.

## Failures

### diagnose_failed

- **audit_experiment** (`17.1`)
  - [38;5;232m[36mℹ[38;5;232m In index: 1.
[36mℹ[38;5;232m With name: design.[39m
[1mCaused by error in `base_map()` at ]8;line = 24:col = 5;file://C:\WZB Dropbox\Macartan Humphreys\5_github\DeclareDesign\R\helpers.RDeclareDesign/R/helpers.R:24:5]8;;:[22m
[38;5;232m[36mℹ[38;5;232m In index: 1.[39m
[1mCaused by error in `names(data) %||% character(0)`:[22m
[33m![39m could not find function "%||%"

- **audit_intervention** (`17.2`)
  - [38;5;232m[36mℹ[38;5;232m In index: 1.
[36mℹ[38;5;232m With name: design.[39m
[1mCaused by error in `base_map()` at ]8;line = 24:col = 5;file://C:\WZB Dropbox\Macartan Humphreys\5_github\DeclareDesign\R\helpers.RDeclareDesign/R/helpers.R:24:5]8;;:[22m
[38;5;232m[36mℹ[38;5;232m In index: 1.[39m
[1mCaused by error in `names(data) %||% character(0)`:[22m
[33m![39m could not find function "%||%"

- **baseline_over_N** (`11.1`)
  - [38;5;232m[36mℹ[38;5;232m In index: 1.
[36mℹ[38;5;232m With name: design.[39m
[1mCaused by error in `base_map()` at ]8;line = 24:col = 5;file://C:\WZB Dropbox\Macartan Humphreys\5_github\DeclareDesign\R\helpers.RDeclareDesign/R/helpers.R:24:5]8;;:[22m
[38;5;232m[36mℹ[38;5;232m In index: 1.[39m
[1mCaused by error in `names(data) %||% character(0)`:[22m
[33m![39m could not find function "%||%"

- **blocked_and_clustered** (`18.5`)
  - [38;5;232m[36mℹ[38;5;232m In index: 1.
[36mℹ[38;5;232m With name: design.[39m
[1mCaused by error in `base_map()` at ]8;line = 24:col = 5;file://C:\WZB Dropbox\Macartan Humphreys\5_github\DeclareDesign\R\helpers.RDeclareDesign/R/helpers.R:24:5]8;;:[22m
[38;5;232m[36mℹ[38;5;232m In index: 1.[39m
[1mCaused by error in `names(data) %||% character(0)`:[22m
[33m![39m could not find function "%||%"

- **cluster_random_sampling** (`15.3`)
  - [38;5;232m[36mℹ[38;5;232m In index: 1.
[36mℹ[38;5;232m With name: design.[39m
[1mCaused by error in `base_map()` at ]8;line = 24:col = 5;file://C:\WZB Dropbox\Macartan Humphreys\5_github\DeclareDesign\R\helpers.RDeclareDesign/R/helpers.R:24:5]8;;:[22m
[38;5;232m[36mℹ[38;5;232m In index: 1.[39m
[1mCaused by error in `names(data) %||% character(0)`:[22m
[33m![39m could not find function "%||%"

- **two_arm_block_cluster**
  - [38;5;232m[36mℹ[38;5;232m In index: 1.
[36mℹ[38;5;232m With name: design.[39m
[1mCaused by error in `base_map()` at ]8;line = 24:col = 5;file://C:\WZB Dropbox\Macartan Humphreys\5_github\DeclareDesign\R\helpers.RDeclareDesign/R/helpers.R:24:5]8;;:[22m
[38;5;232m[36mℹ[38;5;232m In index: 1.[39m
[1mCaused by error in `names(data) %||% character(0)`:[22m
[33m![39m could not find function "%||%"

- **two_arm_block_rdss** (`18.4`)
  - [38;5;232m[36mℹ[38;5;232m In index: 1.
[36mℹ[38;5;232m With name: design.[39m
[1mCaused by error in `base_map()` at ]8;line = 24:col = 5;file://C:\WZB Dropbox\Macartan Humphreys\5_github\DeclareDesign\R\helpers.RDeclareDesign/R/helpers.R:24:5]8;;:[22m
[38;5;232m[36mℹ[38;5;232m In index: 1.[39m
[1mCaused by error in `names(data) %||% character(0)`:[22m
[33m![39m could not find function "%||%"

### missing_packages

- **network_experiment** (`18.13`)
  - Design 'network_experiment' needs packages not installed: interference

## Full table

See `audit_report.csv` / `audit_report.txt` in this folder (problems first).


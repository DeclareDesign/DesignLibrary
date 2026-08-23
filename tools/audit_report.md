# ResearchDesigns audit report

Summary: **66/68** designs OK, **2** failed.

Issue types: `missing_packages`, `yaml_extra_params`, `param_discovery`, `load_error`, `missing_object`, `diagnose_failed`, `disabled`, `other`.

Soft notes (do not fail the audit):
- `no YAML tip`: redesignable param has no tip string in YAML (optional).
- `assigned before design but not redesignable`: top-level `name <- ...` is used by the design but `redesign()` cannot change it (often a fixed vector/data object).
Design steps (`declare_*` pieces) are not parameters. Functions assigned before `design <-` are R-only parameters.
Parked designs (`functional: false`) are listed under Disabled and do not count as failures.
Plain-text listing (`audit_report.txt`) puts FAIL/SKIP first, then OK.

## Failures

### missing_packages

- **italian_village_bayes** (`9.3`)
  - Design 'italian_village_bayes' needs packages not installed: rstanarm

- **process_tracing** (`16.1b`)
  - Design 'process_tracing' needs packages not installed: CausalQueries

## Full table

See `audit_report.csv` / `audit_report.txt` in this folder (problems first).


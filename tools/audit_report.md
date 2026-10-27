# DesignLibrary audit report

Summary: **75/75** designs OK.

Issue types: `missing_packages`, `yaml_extra_params`, `param_discovery`, `load_error`, `missing_object`, `diagnose_failed`, `disabled`, `other`.

Soft notes (do not fail the audit):
- `no YAML tip`: redesignable param has no tip string in YAML (optional).
- `assigned before design but not redesignable`: top-level `name <- ...` is used by the design but `redesign()` cannot change it (often a fixed vector/data object).
Design steps (`declare_*` pieces) are not parameters. Functions assigned before `design <-` are R-only parameters.
Parked designs (`functional: false`) are listed under Disabled and do not count as failures.
Plain-text listing (`audit_report.txt`) puts FAIL/SKIP first, then OK.

## Failures

_None._

## Soft notes (OK designs)

- **factorial**: no YAML tip: factorial_grid, factorial_po_names, factorial_population, factorial_assignment, factorial_reveal, factorial_inquiries, factorial_estimator
- **multiarm_trial**: no YAML tip: per_arm
- **process_tracing_bayes**: no YAML tip: joint_prob, bayes_rule, prior_only, clue_posterior, E1_only, E2_only, both_clues
- **regression_discontinuity_polynomial**: no YAML tip: po_function
- **spillover**: no YAML tip: dgp

## Full table

See `audit_report.csv` / `audit_report.txt` in this folder (problems first).


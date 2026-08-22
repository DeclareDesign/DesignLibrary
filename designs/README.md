# Flattened designs

Three designers in this package build their design text from an argument, so
`k = 3` and `k = 4` are different designs with different numbers of steps and
inquiries. There is no slider for `k`. They cannot become a ResearchDesigns
file, which is a script of top-level assignments followed by `design <- ...`,
without fixing the argument that does the generating.

These are those designers at fixed values, written in the DeclareDesign 2.0
idiom and in the ResearchDesigns file format (YAML front matter, then the
script). Each was checked with `tools/check_flat_designs.R`.

| file | from | fixed at |
|---|---|---|
| `factorial_2x2x2.R` | `factorial_designer` | `k = 3` |
| `multi_arm_three.R` | `multi_arm_designer` | `m_arms = 3` |
| `block_cluster_two_arm.R` | `block_cluster_two_arm_designer` | scalar block, cluster and unit counts |

## What they do not do

The point of a designer is the general case, and flattening gives it up. What
is lost, per design:

**`factorial_2x2x2`.** Only three factors. Assignment probabilities are fixed
at 0.5 for every factor, so the designer's propensity weighting
(`weights = 1/Z_cond_prob`) is a constant and is dropped; a version with
unequal probabilities needs it back. The model is parameterised by marginal
effects and interactions rather than by the `2^k` vector of cell means, which
is the more legible parameterisation for a slider but not the same argument
surface.

**`multi_arm_three`.** Only three arms. The arm-level shocks (`outcome_sds`)
are dropped and only the individual-level shock remains, which is the
designer's own default of `rep(0, m_arms)`. Arms are named `"1"`, `"2"`, `"3"`.

**`block_cluster_two_arm`.** Block, cluster and unit counts are scalars, so
the designer's ragged case (a vector `N_clusters_in_block` or
`N_i_in_cluster`) is gone. `rho`, the correlation between the treated and
untreated individual shocks, is fixed at 1: one individual shock enters both
potential outcomes.

Flattening also drops two things with nowhere to live in a script of top-level
assignments, and both are real losses:

- **Argument validation.** `stop("sd_block must be nonnegative")` and the
  designer's consistency checks between `N` and the level sizes have no home.
- **Derived defaults.** `treatment_mean = control_mean + ate` and
  `sd_block = 0.5773 * sd` cannot be top-level assignments, because
  `redesign()` changes a captured value and does not re-run the script, so a
  slider on `ate` would move `ate` and leave `treatment_mean` behind. The
  derivations are inlined into the design expression instead: the outcome uses
  `ate` directly, and each standard deviation is its own parameter.

## Checked

`tools/check_flat_designs.R` requires four things of each file, not one: it
parses, it runs, it diagnoses, and **every parameter the YAML advertises moves
the data when `redesign()` changes it**. The last is the failure a slider
cannot survive quietly, since the control moves, the diagnosis re-runs, and the
number does not change.

As of 2026-08-18, against DeclareDesign 2.0.0 at `e4000ca`: all three
build, 0 orphan inquiries, every parameter live (8/8, 9/9, 5/5), and no bias
distinguishable from zero at 500 sims.

`factorial_2x2x2` is checked harder, because its estimands are the part most
easily got wrong. Under the defaults the model is `Y = 0.5*T1 + T1*T2*T3 + u`,
for which the designer's documented mapping implies an average marginal effect
of 0.75 for `T1`, 0.25 for `T2` and `T3`, two-way average interactions of 0.5,
a three-way interaction of 1, and an overall average of 0.375. All eight
estimands hit those values exactly, and the demeaned interacted regression
recovers each one.

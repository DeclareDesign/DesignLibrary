---
id: trust_game
alias: "17.6"
label: trust game
category: rdss
keywords: [experiment, descriptive]
description: >
  Trust game design. (chapter 17).
params:
  "deceive": "Whether the trust-game design allows deception"
  "n_pairs": "Number of pairs"
  "invested": "Function: first-mover investment given types a_1 and a_2 (R-only)"
  "average_invested": "Function: expected investment for a type, averaging over partners (R-only)"
  "returned": "Function: second-mover return given investment and type a_2 (R-only)"
  "average_returned": "Function: expected return for a type, averaging over investments (R-only)"
book_link: https://book.declaredesign.org/library/experimental-descriptive.html#def-ch17num6
include_in_shiny: true
---


invested <- function(a_1, a_2) {
  u_a = (1 - a_1) * log(1 - a_1) + a_1 * log(2 * a_1)  # give a1
  u_b = (1 - a_1) * log(2 * a_2) + a_1 * log(2 * (1 - a_2)) # give 1
  ifelse(u_a > u_b, a_1, 1)
}

average_invested <- function(a_1) 
  mean(sapply(seq(0, 1, .01),  invested, a_1 = a_1))

returned <- function(x1, a_2 = 1/3) 
  ((2 * a_2 * x1 - (1 - a_2) * (1 - x1)) / (2 * x1)) * 
  (x1  > (1 - a_2) / (1 + a_2))

average_returned <- function(a_2) 
  mean(sapply(seq(0.01, 1, .01), returned, a_2 = a_2))

design <-
  declare_parameters(
    n_pairs = 200,
    deceive = FALSE
  ) +
  
  declare_model(N = 2 * n_pairs,
                a = runif(N)) +
  
  declare_inquiries(
    trusting = mean(sapply(a, average_invested)),
    trustworthy = mean(sapply(a, average_returned))) +

  declare_assignment(pair = complete_ra(N = N, num_arms = n_pairs),
                     role = 1 + block_ra(blocks = pair)) + 
  # Quoted column names: DeclareDesignZero evaluates step args as values;
  # tidyr::pivot_wider needs tidyselect (expressions / names). See notes_zero.
  declare_step(
    id_cols = "pair",
    names_from = "role",
    values_from = c("ID", "a"),
    handler = tidyr::pivot_wider) +
  
  declare_measurement(invested = invested(a_1, a_2)) + 
  
  declare_estimator(
    invested ~ 1,
    .method = lm_robust,
    inquiry = "trusting",
    label = "trusting") +

  declare_measurement(invested = deceive*runif(N) + (1-deceive)*invested,
                      returned = returned(invested, a_2)) +
  
  declare_estimator(
    returned ~ 1,
    .method = lm_robust,
    inquiry = "trustworthy",
    label = "trustworthy")

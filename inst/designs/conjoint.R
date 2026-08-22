---
id: conjoint
alias: "17.5"
label: conjoint
category: rdss
keywords: [experiment, descriptive]
description: >
  Conjoint experiment design. (chapter 17).
packages: [rdss, cjoint, dplyr, purrr]
params:
  "N_subjects": "Number of subjects"
  "N_tasks": "Number of conjoint tasks per subject"
  "conjoint_utility": "Function: maps conjoint profile features to utility (R-only)"
book_link: https://book.declaredesign.org/library/experimental-descriptive.html#def-ch17num5
include_in_shiny: true
---


# Attributes and levels
levels_list =
  list(
    gender = c("Man", "Woman"),
    party = c("Left", "Right"),
    region = c("North", "South", "East", "West")
  )

# Conjectured utility function
conjoint_utility <-
  function(data){
    data |>
      mutate(U = 0.25*(gender == "Woman")*(region %in% c("North", "East")) +
               0.5*(party == "Right")*(region %in% c("North", "South")) + uij)
  }

design <-
  declare_parameters(
    N_subjects = 500,
    N_tasks = 3
  ) +
  declare_model(
    subject = add_level(N = N_subjects),
    task = add_level(N = N_tasks, task = 1:N_tasks),
    profile = add_level(
      N = 2,
      profile = 1:2,
      uij = rnorm(N, sd = 1)
    )
  ) +
  declare_inquiry(handler = conjoint_inquiries,
                  levels_list = levels_list,
                  utility_fn = conjoint_utility) +
  declare_assignment(handler = conjoint_assignment,
                     levels_list = levels_list) +
  declare_measurement(handler = conjoint_measurement,
                      utility_fn = conjoint_utility) +
  declare_estimator(choice ~ gender + party + region,
                    respondent.id = "subject",
                    .method = amce)

---
id: process_tracing
alias: "16.1b"
label: process tracing
category: rdss
keywords: [observational, causal]
description: >
  Process tracing design declaration (chapter 16).
packages: [rdss, CausalQueries]
params:
  "query": "Causal query string evaluated by the estimator"
  "strategies": "Process-tracing evidence strategies (e.g. X-Y, X-Y-M)"
diagnosands: [rmse]
book_link: https://book.declaredesign.org/library/observational-causal.html#def-ch16num1
include_in_shiny: true
---

causal_model <- make_model("X -> M -> Y <- W -> M") |>
  set_restrictions("(M[X=1] < M[X=0]) | (M[X=1, W=1] == M[X=0, W=1])") |>
  set_restrictions("(Y[M=1] < Y[M=0]) | (Y[M=1, W=1] == Y[M=0, W=1])")

design <-
  declare_parameters(
    strategies = c("X-Y", "X-Y-M", "X-Y-W",  "X-Y-W-M"),
    query = "Y[X=1] - Y[X=0]"
  ) +
  declare_model(draw_causal_type(causal_model)) +
  declare_inquiry(
    CoE =  query_distribution(
      causal_model,
      query = query,
      parameters = causal_type)) +
  declare_measurement(
    handler = function(data)
      causal_model |>
      make_data(parameters = data$causal_type))  +
  declare_estimator(
    handler = label_estimator(process_tracing_estimator),
    causal_model = causal_model,
    query = query,
    strategies = strategies)

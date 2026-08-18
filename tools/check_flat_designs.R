# Verify the flattened designs in designs/ against DeclareDesign 2.0 ----
#
# A flattened design has to clear four bars, not one: it parses, it runs, it
# diagnoses, and every parameter the YAML advertises actually moves the
# diagnosis when redesign() changes it. The fourth is the one a slider UI
# cannot survive failing silently.

library(fabricatrZero)
library(DeclareDesignZero)
library(randomizr)
library(estimatrZero)
library(tidyverse)

# Report which engine answered ----
for (p in c("DeclareDesignZero", "fabricatrZero", "estimatrZero", "randomizr")) {
  print(paste0(p, " ", packageDescription(p)$Version, " @ ", dirname(system.file(package = p))))
}

design_dir <- file.path(here::here(), "designs")

# Split YAML front matter from the R body ----
read_design_file <- function(path) {
  lines <- read_lines(path)
  fence <- which(str_detect(lines, "^---\\s*$"))
  yaml_text <- lines[(fence[1] + 1):(fence[2] - 1)]
  body <- lines[(fence[2] + 1):length(lines)]
  list(yaml = yaml::yaml.load(paste(yaml_text, collapse = "\n")),
       body = paste(body, collapse = "\n"))
}

# Source the body into a fresh environment and hand back the design ----
build_design <- function(body) {
  env <- new.env(parent = globalenv())
  eval(parse(text = body), envir = env)
  list(design = get("design", envir = env), env = env)
}

check_one <- function(path) {
  parts <- read_design_file(path)
  built <- build_design(parts$body)
  design <- built$design

  n_steps <- length(design)
  dat <- draw_data(design)
  one_run <- run_design(design)
  dx <- diagnose_design(design, sims = 500, bootstrap_sims = 200)
  dx_df <- dx$diagnosands_df

  # Every inquiry should meet an estimate; an orphan means a broken mapping
  orphan_inquiries <- setdiff(unique(one_run$inquiry[!is.na(one_run$inquiry)]),
                              unique(one_run$inquiry[!is.na(one_run$estimate)]))

  # Bias should be indistinguishable from zero if the estimands are right
  worst_bias <- dx_df |>
    mutate(z = abs(bias) / `se(bias)`) |>
    slice_max(z, n = 1)

  # Does redesign() on each advertised parameter actually change anything?
  # Both draws are taken under the same seed, so any difference is the
  # parameter and not the RNG. A parameter that does not move the data here
  # is a slider that would move and change nothing.
  params <- names(parts$yaml$params)
  draw_under_seed <- function(d) {
    set.seed(20260818)
    draw_data(d)
  }
  baseline <- draw_under_seed(design)
  param_moves <- map_lgl(params, function(p) {
    old_value <- get(p, envir = built$env)
    # A probability has to be perturbed to another probability, or the check
    # reports a live parameter as dead because the design rejected the value.
    new_value <- if (is.numeric(old_value) && old_value > 0 && old_value < 1) {
      old_value / 2
    } else if (is.numeric(old_value) && old_value != 0) {
      old_value * 2 + 1
    } else {
      7
    }
    tryCatch({
      d2 <- do.call(redesign, c(list(design), setNames(list(new_value), p)))
      !identical(draw_under_seed(d2), baseline)
    }, error = function(e) NA)
  })
  names(param_moves) <- params

  tibble(
    design = parts$yaml$id,
    steps = n_steps,
    n_rows = nrow(dat),
    inquiries = length(unique(one_run$inquiry[!is.na(one_run$inquiry)])),
    orphans = length(orphan_inquiries),
    max_bias_z = round(worst_bias$z, 2),
    max_bias_on = worst_bias$inquiry,
    params = length(params),
    params_live = sum(param_moves, na.rm = TRUE),
    dead_params = paste(names(param_moves)[!param_moves %in% TRUE], collapse = ", ")
  )
}

set.seed(20260818)
paths <- list.files(design_dir, pattern = "\\.R$", full.names = TRUE)
results <- map(paths, check_one) |> list_rbind()
print(as.data.frame(results))

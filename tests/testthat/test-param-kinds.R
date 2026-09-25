test_that("get_args classifies scalar, vector, and data", {
  skip_if_not_installed("DeclareDesign")
  skip_on_cran()

  args <- get_args("simple_random_sampling")
  expect_true("kind" %in% names(args))
  expect_true("shiny" %in% names(args))
  expect_true("dataset" %in% args$name)
  expect_equal(args$kind[args$name == "dataset"], "data")
  expect_false(isTRUE(args$shiny[args$name == "dataset"]))
  expect_true(all(args$shiny[args$name %in% c("cuts", "sample_size")]))

  pop <- fabricatr::fabricate(N = 121, Y_star = rnorm(N, mean = 10))
  d <- make_design("simple_random_sampling", dataset = pop)
  expect_true(all(DeclareDesign::draw_data(d)$Y_star %in% pop$Y_star))

  vargs <- get_args("two_by_two")
  expect_equal(vargs$kind[vargs$name == "outcome_means"], "vector")
  expect_true(isTRUE(vargs$shiny[vargs$name == "outcome_means"]))
  expect_equal(vargs$kind[vargs$name == "N"], "scalar")
})

test_that("get_args exposes functions as R-only knobs on multiarm_trial", {
  skip_if_not_installed("DeclareDesign")
  skip_on_cran()

  args <- get_args("multiarm_trial")
  expect_true(all(
    c("N", "m_arms", "outcome_means", "outcome_sds", "sd_i", "conditions", "Y") %in%
      args$name
  ))
  expect_equal(args$kind[args$name == "Y"], "function")
  expect_false(isTRUE(args$shiny[args$name == "Y"]))
  expect_true(is.function(args$default[args$name == "Y"][[1]]))

  d <- make_design(
    "multiarm_trial",
    N = 24,
    Y = function(Z, u, outcome_sds) as.numeric(Z) + u
  )
  expect_s3_class(d, "design")
  dat <- DeclareDesign::draw_data(make_design("multiarm_trial", N = 24))
  expect_true("Y" %in% names(dat))
  expect_equal(nrow(dat), 24L)
})

test_that("get_args succeeds when find_all_objects errors on empty names", {
  skip_if_not_installed("DeclareDesign")
  # `multilevel` and `multilevel_answer_strategies` declare these in YAML
  # `packages:`, and eval_design() errors rather than skipping without them.
  skip_if_not_installed("rdss")
  skip_if_not_installed("lme4")
  skip_on_cran()

  for (id in c("latent_variables", "multilevel", "multilevel_answer_strategies")) {
    args <- get_args(id)
    expect_true(is.data.frame(args), info = id)
  }
  expect_true("N" %in% get_args("latent_variables")$name)
})

test_that("classify_param_kind treats functions as R-only", {
  expect_equal(ResearchDesigns:::classify_param_kind(function(x) x), "function")
  expect_false(ResearchDesigns:::is_shiny_param_kind("function"))
  expect_true(ResearchDesigns:::is_modifiable_value(function(x) x))
})

test_that("make_design replaces a vector parameter as one value", {
  skip_if_not_installed("DeclareDesign")
  skip_on_cran()

  d <- make_design(
    "two_by_two",
    N = 40,
    sd_i = 0.0001,
    outcome_sds = c(0, 0, 0, 0),
    outcome_means = c(0, 0, 0, 1)
  )
  expect_s3_class(d, "design")
  est <- DeclareDesign::draw_estimands(d)
  interaction <- est$estimand[est$inquiry == "interaction"]
  expect_equal(as.numeric(interaction), 1, tolerance = 1e-6)
})

test_that("conditional_expectation lists dip and polynomial_degrees", {
  skip_if_not_installed("DeclareDesign")
  skip_on_cran()

  args <- get_args("conditional_expectation")
  expect_true(all(
    c("dip", "polynomial_degrees", "N", "x_range") %in% args$name
  ))
  expect_equal(unname(args$kind[args$name == "dip"]), "function")
  expect_false(isTRUE(args$shiny[args$name == "dip"]))
  expect_true(is.function(args$default[[which(args$name == "dip")]]))
  expect_equal(unname(args$kind[args$name == "polynomial_degrees"]), "vector")
  expect_true(isTRUE(args$shiny[args$name == "polynomial_degrees"]))
  expect_true(all(nzchar(args$tip) & !is.na(args$tip)))

  expect_equal(ResearchDesigns:::classify_param_kind(1:4), "vector")

  parsed <- ResearchDesigns:::resolve_design("conditional_expectation")
  d0 <- ResearchDesigns:::eval_design(parsed)
  params <- ResearchDesigns:::discover_design_params(d0, code = parsed$code)
  dots <- ResearchDesigns:::prepare_redesign_dots(
    params,
    list(polynomial_degrees = 1:4)
  )
  expect_true(is.list(dots$polynomial_degrees))
  expect_equal(dots$polynomial_degrees[[1]], 1:4)

  d <- make_design("conditional_expectation", polynomial_degrees = 1:4)
  expect_s3_class(d, "design")
  expect_false(is.list(d) && !inherits(d, "design"))

  v <- ResearchDesigns:::validate_params_against_design(
    parsed$meta, d0, code = parsed$code
  )
  expect_true(v$ok)
  expect_equal(v$extra_docs, character(0))
  expect_equal(v$missing_docs, character(0))

  meta_extra <- parsed$meta
  meta_extra$params <- c(meta_extra$params, list(not_a_param = "nope"))
  v_extra <- ResearchDesigns:::validate_params_against_design(
    meta_extra, d0, code = parsed$code
  )
  expect_false(v_extra$ok)
  expect_true("not_a_param" %in% v_extra$extra_docs)
})

test_that("Shiny parser treats vector commas as replacement, not a sweep", {
  p <- ResearchDesigns:::parse_shiny_param_raw(
    "0, 0, 0, 1", "vector", "0, 0, 0, 0"
  )
  expect_false(isTRUE(p$skip))
  expect_false(isTRUE(p$sweep))
  expect_equal(p$value, c(0, 0, 0, 1))

  sweep <- ResearchDesigns:::parse_shiny_param_raw(
    "0,0,0,1; 0,0.5,0.5,1", "vector", ""
  )
  expect_true(isTRUE(sweep$sweep))
  expect_equal(length(sweep$value), 2L)

  sc <- ResearchDesigns:::parse_shiny_param_raw("50, 100", "scalar", "100")
  expect_true(isTRUE(sc$sweep))
  expect_equal(sc$value, c(50, 100))
})

n_li <- function(html) {
  m <- gregexpr("<li>", html, fixed = TRUE)[[1]]
  if (length(m) == 1L && identical(m[[1]], -1L)) 0L else length(m)
}

test_that("vector defaults display a trailing semicolon; scalars do not", {
  expect_equal(
    ResearchDesigns:::format_shiny_param_default(c(0, 0, 0), "vector"),
    "0, 0, 0;"
  )
  expect_equal(
    ResearchDesigns:::format_shiny_param_default(c(0, 0, 0), "vector", "c(0, 0, 0)"),
    "0, 0, 0;"
  )
  expect_equal(
    ResearchDesigns:::format_shiny_param_default(100, "scalar", "100"),
    "100"
  )
  expect_false(grepl(";", ResearchDesigns:::format_shiny_param_default(100, "scalar", "100"), fixed = TRUE))
})

test_that("a trailing semicolon on a vector is not a sweep", {
  skip_same <- ResearchDesigns:::parse_shiny_param_raw(
    "0, 0, 0;", "vector", "0, 0, 0;"
  )
  expect_true(isTRUE(skip_same$skip))

  skip_hint <- ResearchDesigns:::parse_shiny_param_raw(
    "0, 0, 0;", "vector", "0, 0, 0"
  )
  expect_true(isTRUE(skip_hint$skip))

  trail <- ResearchDesigns:::parse_shiny_param_raw("0, 0, 0;", "vector", "1, 1, 1;")
  expect_false(isTRUE(trail$skip))
  expect_false(isTRUE(trail$sweep))
  expect_equal(trail$value, c(0, 0, 0))

  sweep <- ResearchDesigns:::parse_shiny_param_raw(
    "0, 0, 0; 0.1, 0.2, 0.3", "vector", "0, 0, 0;"
  )
  expect_true(isTRUE(sweep$sweep))
  expect_equal(length(sweep$value), 2L)
})

test_that("redesign_kind_help is a short conditional bullet list", {
  two <- data.frame(
    name = c("N", "ate"),
    kind = c("scalar", "scalar"),
    stringsAsFactors = FALSE
  )
  two$default <- I(list(100, 1))
  html_two <- ResearchDesigns:::redesign_kind_help("two_arm_flexible", args = two)
  expect_match(html_two, "^<ul><li>")
  expect_equal(n_li(html_two), 1L)
  expect_match(html_two, "You can change parameter values below")
  expect_false(grepl("On a vector parameter", html_two, fixed = TRUE))
  expect_false(grepl("Note:", html_two, fixed = TRUE))
  expect_false(grepl("Functions, data frames", html_two, fixed = TRUE))

  multi <- data.frame(
    name = c("N", "m_arms", "outcome_means", "Y"),
    kind = c("scalar", "scalar", "vector", "function"),
    stringsAsFactors = FALSE
  )
  multi$default <- I(list(90, 3, c(0, 0, 0), function(Z) Z))
  html_m <- ResearchDesigns:::redesign_kind_help("multiarm_trial", args = multi)
  expect_equal(n_li(html_m), 3L)
  expect_match(html_m, "You can change parameter values below")
  expect_match(html_m, "On a vector parameter")
  expect_match(html_m, "outcome_means: 0, 0, 0; 0.1, 0.2, 0.3")
  expect_false(grepl("Note:", html_m, fixed = TRUE))
  expect_match(html_m, "Y = \\.\\.\\.")
  expect_match(html_m, 'make_design\\("multiarm_trial"')
})

test_that("redesign_kind_help matches two_arm_flexible and multiarm_trial kinds", {
  skip_if_not_installed("DeclareDesign")
  skip_on_cran()

  html_two <- ResearchDesigns:::redesign_kind_help("two_arm_flexible")
  expect_equal(n_li(html_two), 1L)
  expect_match(html_two, "You can change parameter values below")
  expect_false(grepl("On a vector parameter", html_two, fixed = TRUE))
  expect_false(grepl("Note:", html_two, fixed = TRUE))
  expect_false(grepl("Functions, data frames", html_two, fixed = TRUE))

  html_m <- ResearchDesigns:::redesign_kind_help("multiarm_trial")
  expect_equal(n_li(html_m), 3L)
  expect_match(html_m, "On a vector parameter")
  expect_match(html_m, "outcome_means")
  expect_false(grepl("Note:", html_m, fixed = TRUE))
  expect_match(html_m, "Y = \\.\\.\\.")
})

test_that("classify_param_kind separates a bare list from a classed one", {
  expect_equal(ResearchDesigns:::classify_param_kind(list(a = 1, b = "x")), "list")
  expect_true(ResearchDesigns:::is_modifiable_value(list(a = 1)))
  expect_false(ResearchDesigns:::is_shiny_param_kind("list"))
  expect_equal(ResearchDesigns:::classify_param_kind(data.frame(a = 1)), "data")
  expect_equal(ResearchDesigns:::classify_param_kind(y ~ x), "data")
  expect_false(ResearchDesigns:::is_modifiable_value(y ~ x))
})

test_that("a list of lists is a sweep and anything else is one value", {
  expect_true(ResearchDesigns:::is_list_sweep(list(list(a = 1), list(a = 2))))
  expect_true(ResearchDesigns:::is_list_sweep(list(list(a = 1))))
  expect_false(ResearchDesigns:::is_list_sweep(list(a = 1, b = 2)))
  expect_false(ResearchDesigns:::is_list_sweep(list()))
  expect_false(ResearchDesigns:::is_list_sweep(c(1, 2)))
  expect_false(ResearchDesigns:::is_list_sweep(list(data.frame(a = 1))))
})

test_that("conjoint exposes levels_list and redesigns it whole", {
  skip_if_not_installed("DeclareDesign")
  skip_if_not_installed("rdss")
  skip_if_not_installed("cjoint")
  skip_on_cran()

  args <- get_args("conjoint")
  expect_true("levels_list" %in% args$name)
  i <- which(args$name == "levels_list")
  expect_equal(unname(args$kind[i]), "list")
  expect_false(isTRUE(args$shiny[i]))
  expect_true(is.list(args$default[[i]]))
  expect_true(all(nzchar(args$tip) & !is.na(args$tip)))

  levels_list <- list(
    gender = c("Man", "Woman", "Nonbinary"),
    party = c("Left", "Right"),
    region = c("North", "South", "East", "West")
  )
  d <- make_design("conjoint", levels_list = levels_list, N_subjects = 20)
  expect_s3_class(d, "design")
  expect_true("Nonbinary" %in% DeclareDesign::draw_data(d)$gender)

  swept <- make_design(
    "conjoint",
    levels_list = list(levels_list, levels_list),
    N_subjects = 20
  )
  expect_length(swept, 2L)
  expect_s3_class(swept[[1]], "design")
})

test_that("get_args prints a table holding a function and a list", {
  skip_if_not_installed("DeclareDesign")
  skip_if_not_installed("rdss")
  skip_if_not_installed("cjoint")
  skip_on_cran()

  args <- get_args("conjoint")
  expect_s3_class(args, "research_designs_args")
  out <- capture.output(print(args))
  expect_true(any(grepl("levels_list", out)))
  expect_true(any(grepl("conjoint_utility", out)))
  # `I(list())` here made the table unprintable: format.AsIs() runs toString()
  # over the column and a closure cannot be coerced.
  expect_false(inherits(args$default, "AsIs"))
})

test_that("a vector default too long for Shiny is still replaced whole, not swept", {
  args <- get_args("conditional_expectation")
  expect_equal(unname(args$kind[args$name == "x_range"]), "data")

  x <- seq(0, 2, length.out = 50)
  expect_no_warning(d <- make_design("conditional_expectation", x_range = x))
  expect_s3_class(d, "design")

  sweep <- make_design("conditional_expectation", x_range = list(x, 2 * x))
  expect_length(sweep, 2)
})

# The contract every designer must satisfy. Each designer's own test file
# calls test_designer_contract() once, naming by hand any argument that
# redesign() cannot reach, so that list is visible where the designer is tested.

definition_columns <- c("names", "tips", "class", "vector", "min", "max",
                        "inspector_min", "inspector_step")

# Defaults may read other formals (mean_A0B1 = outcome_means[2]), so they are
# evaluated as R would, as promises in one environment. do.call() inlines each
# default's expression rather than `formals(designer)[[f]]`.
designer_defaults <- function(designer) {
  env <- new.env()
  for (f in names(formals(designer))) {
    do.call(delayedAssign, list(f, formals(designer)[[f]],
                                eval.env = env, assign.env = env))
  }
  env
}

# TRUE when redesign() changing `param` to `value` changes a seeded run.
redesign_moves_run <- function(design, param, value) {
  redesigned <- withCallingHandlers(
    do.call(redesign, c(list(design), setNames(list(value), param))),
    warning = function(w) {
      if (grepl("not found in the design", conditionMessage(w))) invokeRestart("muffleWarning")
    })
  set.seed(343)
  before <- run_design(design)
  set.seed(343)
  after <- run_design(redesigned)
  !isTRUE(all.equal(before, after))
}

test_designer_contract <- function(designer_name, redesign_exempt = character(0)) {
  designer <- get(designer_name, envir = asNamespace("DesignLibrary"))
  design <- designer()
  shiny_arguments <- attr(designer, "shiny_arguments")
  definitions <- attr(designer, "definitions")

  test_that(paste(designer_name, "returns a design carrying its code"), {
    expect_s3_class(design, "design")
    expect_type(attr(design, "code"), "character")
    expect_identical(get_design_code(design), attr(design, "code"))
  })

  test_that(paste(designer_name, "'s code evaluates to the design it describes"), {
    env <- new.env(parent = globalenv())
    eval(parse(text = attr(design, "code")), envir = env)
    design_name <- sub("_designer$", "_design", designer_name)
    expect_true(exists(design_name, envir = env, inherits = FALSE))
    rebuilt <- get(design_name, envir = env)
    expect_s3_class(rebuilt, "design")
    expect_gt(nrow(draw_estimates(rebuilt)), 0)
  })

  test_that(paste(designer_name, "'s default design diagnoses"), {
    diagnosis <- diagnose_design(design, sims = 10, bootstrap_sims = FALSE)
    expect_s3_class(diagnosis$diagnosands_df, "data.frame")
    expect_gt(nrow(diagnosis$diagnosands_df), 0)
  })

  test_that(paste(designer_name, "builds with every argument fixed"), {
    expect_no_error(designer(args_to_fix = names(formals(designer))))
  })

  test_that(paste(designer_name, "'s definitions describe exactly its formals"), {
    expect_s3_class(definitions, "data.frame")
    expect_setequal(definitions$names, names(formals(designer)))
    expect_equal(nrow(definitions), length(formals(designer)))
    expect_in(definitions$class, c("character", "numeric", "integer", "logical"))
    expect_in(names(definitions), definition_columns)
  })

  test_that(paste(designer_name, "'s shiny_arguments are documented formals"), {
    expect_type(shiny_arguments, "list")
    expect_in(names(shiny_arguments), names(formals(designer)))
    expect_in(names(shiny_arguments), definitions$names)
  })

  test_that(paste(designer_name, "'s estimates do not share a name with an argument"), {
    expect_length(intersect(definitions$names, names(draw_estimates(design))), 0)
  })

  test_that(paste(designer_name, "'s shiny_arguments move the design under redesign()"), {
    expect_in(redesign_exempt, names(shiny_arguments))
    defaults <- designer_defaults(designer)
    for (param in names(shiny_arguments)) {
      default <- tryCatch(get(param, envir = defaults), error = function(e) NULL)
      values <- Filter(function(v) !isTRUE(all.equal(v, default)), shiny_arguments[[param]])
      if (!length(values)) next
      moves <- redesign_moves_run(design, param, values[[1]])
      # An exempt argument must stay exempt, so the list cannot go stale.
      expect_identical(moves, !param %in% redesign_exempt, label = paste0("redesign moves ", param))
    }
  })
}

# Freeze what the 19 designers do today, before the rewrite changes any of it.
#
# Two tiers, because they answer different questions.
#
# Tier 1 is the parameter surface and the design's shape: formals and their
# defaults, step count, step types, inquiry and estimator labels. It is
# portable across engines, so it is the tier that survives DeclareDesign 2.0.
#
# Tier 2 is a seeded diagnosis under whichever engine is attached. It pins that
# a change to DESIGNER code does not move the numbers while the ENGINE is held
# fixed. It is not comparable across engines: 1.1.1 and 2.0 do not make the
# same RNG calls in the same order, so a difference there would mean nothing.

# Load the package from THIS tree, not whatever is installed. The first run of
# this harness took `library(DesignLibrary)` and got CRAN 0.1.10, which is
# behind master: master's two_arm_designer is already written in the 2.0 idiom
# and CRAN's is not, so the baseline described a different package than the one
# the rewrite starts from. Set DL_LIB to a library holding this tree's build.
if (nzchar(Sys.getenv("DL_LIB"))) .libPaths(c(Sys.getenv("DL_LIB"), .libPaths()))
library(DesignLibrary)
cat("DesignLibrary loaded from:", dirname(getNamespaceInfo("DesignLibrary", "path")), "\n")

engine <- c(
  DeclareDesign = as.character(utils::packageVersion("DeclareDesign")),
  randomizr     = as.character(utils::packageVersion("randomizr")),
  fabricatr     = as.character(utils::packageVersion("fabricatr")),
  estimatr      = as.character(utils::packageVersion("estimatr")),
  DesignLibrary = as.character(utils::packageVersion("DesignLibrary"))
)

# Which package is actually answering. The masking trap is real here:
# DesignLibrary Depends on DeclareDesign, so attaching it attaches the
# original, and a run that claims to test a rewrite can silently be testing
# the parent. Recorded rather than assumed.
answering <- environmentName(environment(DeclareDesign::diagnose_design))

designers <- sort(grep("designer$", getNamespaceExports("DesignLibrary"), value = TRUE))

describe_formals <- function(fn) {
  fm <- formals(fn)
  # Never bind fm[[nm]] to a name: an argument with no default is the empty
  # symbol, and touching it afterwards raises "argument is missing". deparse()
  # takes it directly and returns "" , which is the signal we want.
  vapply(names(fm), function(nm) {
    txt <- paste(deparse(fm[[nm]]), collapse = " ")
    if (!nzchar(trimws(txt))) "<none>" else txt
  }, character(1))
}

describe_shape <- function(design) {
  steps <- unclass(design)
  list(
    n_steps     = length(steps),
    step_types  = vapply(steps, function(s) attr(s, "step_type") %||% NA_character_, character(1)),
    causal_type = vapply(steps, function(s) attr(s, "causal_type") %||% NA_character_, character(1)),
    labels      = names(steps)
  )
}
`%||%` <- function(x, y) if (is.null(x)) y else x

tier1 <- list(); tier2 <- list(); errors <- list()

for (d in designers) {
  fn <- get(d, asNamespace("DesignLibrary"))
  entry <- list(designer = d, formals = describe_formals(fn))

  built <- tryCatch(fn(), error = function(e) e)
  if (inherits(built, "error")) {
    entry$build_error <- conditionMessage(built)
    errors[[d]] <- paste("build:", conditionMessage(built))
    tier1[[d]] <- entry
    next
  }
  entry$shape <- describe_shape(built)
  entry$has_diagnosands_attr <- !is.null(attr(built, "diagnosands"))
  tier1[[d]] <- entry

  set.seed(20260818)
  dx <- tryCatch(
    suppressWarnings(DeclareDesign::diagnose_design(built, sims = 30, bootstrap_sims = 0)),
    error = function(e) e
  )
  if (inherits(dx, "error")) {
    errors[[d]] <- paste("diagnose:", conditionMessage(dx))
    next
  }
  tier2[[d]] <- dx$diagnosands_df
}

dir.create("tests/baseline", showWarnings = FALSE, recursive = TRUE)
saveRDS(
  list(engine = engine, answering = answering, tier1 = tier1, tier2 = tier2, errors = errors),
  "tests/baseline/designers_baseline.rds"
)

cat("engine answering:", answering, "\n")
print(engine)
cat("\ndesigners:", length(designers),
    "| built:", length(tier1) - sum(vapply(tier1, function(x) !is.null(x$build_error), logical(1))),
    "| diagnosed:", length(tier2),
    "| errors:", length(errors), "\n\n")
if (length(errors)) for (nm in names(errors)) cat(" ", nm, "->", errors[[nm]], "\n")

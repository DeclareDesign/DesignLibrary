# Port the DesignLibrary designers to DeclareDesign 2.0, and measure how far
# mechanical rules get. Nothing here is committed to R/: this is the harness
# that decides whether a real port is worth doing, and what it would cost.
#
# WHY IT EXISTS. DesignLibrary cannot load against DeclareDesign 2.0 at all:
# of the 18 names it importFrom()s, declare_potential_outcomes and
# declare_reveal are not exported by 2.0, and an importFrom on a name a
# namespace does not export is a load-time error. So every designer has to be
# rewritten or the package has to be archived. This script measures which.
#
# HOW TO RUN. Needs a library holding DeclareDesign 2.0 and its family
# (install DeclareDesign@rewrite, fabricatr@rewrite, estimatr@rewrite), plus a
# library holding THIS tree's DesignLibrary build for the designers' formals:
#
#   R CMD INSTALL -l /tmp/dl1x .
#   SPLIB=/tmp/rlib DL_LIB=/tmp/dl1x Rscript tools/port_to_dd2.R
#
# WHAT IT FOUND, 2026-08-18, against DeclareDesign 2.0 at e4000ca:
# 7 of 15 run after the rules below, up from 1 with no rules. Every remaining
# failure is a gap in this transform or an artifact of evaluating a designer
# body outside its function (`object 'population_expr' not found`), NOT a 2.0
# capability gap. A real port should clear all 15.
#
# THE RULES, each verified to work under 2.0 on its own before being applied:
#   declare_population(X)                   -> declare_model(X)
#   declare_potential_outcomes(<formula>)   -> declare_model(potential_outcomes(<formula>))
#   declare_potential_outcomes(Y_Z_0 = ...) -> declare_model(Y_Z_0 = ...)
#   declare_reveal(Y, Z)                    -> declare_measurement(Y = reveal_outcomes(Y ~ Z))
#   inquiry = <step object>                 -> inquiry = "ATE"
# The last is not about potential outcomes at all and accounted for 10 of the
# 15 initial failures on its own.
#
# WHAT DOES NOT PORT TO A ResearchDesigns FILE, which is a different question
# from whether it runs. Three designers build their design text from an
# argument: factorial_designer (k factors, 25 metaprogramming calls),
# multi_arm_designer (m_arms, 17) and block_cluster_two_arm_designer (7). For
# those, k = 3 and k = 4 are different designs with different numbers of steps
# and inquiries, so there is no slider for k. They stay functions, or ship as
# several fixed files, or ResearchDesigns grows a design-generator notion.
# The other 12 flatten cleanly. Flattening does lose two things worth naming:
# argument validation (stop("control_sd must be non-negative")) and derived
# defaults (treatment_mean = control_mean + ate), neither of which has a home
# in a script of top-level assignments.

.libPaths(c(Sys.getenv("SPLIB"), .libPaths()))
suppressMessages({library(fabricatr); library(DeclareDesign); library(randomizr); library(estimatr)})
dl <- Sys.getenv("DL_LIB"); ns <- loadNamespace("DesignLibrary", lib.loc = dl)
# Read the designers out of this repository rather than a scratch copy.
files <- list.files("R", pattern = "designer\\.R$", full.names = TRUE)

extract_body <- function(path) {
  ln <- readLines(path, warn = FALSE)
  a <- grep("\\{\\{\\{", ln)[1]; b <- grep("\\}\\}\\}", ln)[1]
  if (is.na(a) || is.na(b)) return(NULL)
  paste(ln[(a + 1):(b - 1)], collapse = "\n")
}

# names -> a one-sided sum, Y ~ A + B
as_rhs <- function(x) {
  if (is.character(x)) return(lapply(x, as.name))
  if (is.call(x) && identical(x[[1]], as.name("c"))) return(lapply(as.list(x)[-1], function(e)
    if (is.character(e)) as.name(e) else e))
  list(x)
}
build_reveal <- function(outs, assigns) {
  rhs <- Reduce(function(a, b) call("+", a, b), assigns)
  args <- lapply(outs, function(o) call("reveal_outcomes", call("~", o, rhs)))
  names(args) <- vapply(outs, deparse, character(1))
  as.call(c(list(as.name("declare_measurement")), args))
}

rewrite <- function(e) {
  if (!is.call(e)) return(e)
  fn <- e[[1]]
  if (is.name(fn)) {
    f <- as.character(fn)
    if (f == "eval_bare") { e[[1]] <- quote(rlang::eval_bare) }
    else if (f == "declare_population") { e[[1]] <- as.name("declare_model") }
    else if (f == "declare_potential_outcomes") {
      a <- as.list(e)[-1]; nm <- names(a) %||% rep("", length(a))
      # Two forms. The formula form needs fabricatr's potential_outcomes(); the
      # explicitly-named form (Y_Z_0 = ..., Y_Z_1 = ...) does not, because in
      # 2.0 those are just ordinary model variables.
      if (length(a) && all(nzchar(nm))) {
        out <- e; out[[1]] <- as.name("declare_model")
        return(as.call(lapply(as.list(out), rewrite)))
      }
      inner <- e; inner[[1]] <- as.name("potential_outcomes")
      return(call("declare_model", rewrite(inner)))
    }
    else if (f == "declare_reveal") {
      a <- as.list(e)[-1]; nms <- names(a) %||% rep("", length(a))
      outs <- NULL; assigns <- NULL
      for (i in seq_along(a)) {
        n <- nms[i]
        if (n %in% c("", "Y_variables", "outcome_variables")) {
          if (n == "" && !is.null(outs)) assigns <- as_rhs(a[[i]]) else outs <- as_rhs(a[[i]])
        } else if (n %in% c("assignment_variable", "assignment_variables", "Z")) {
          assigns <- as_rhs(a[[i]])
        }
      }
      if (is.null(outs))    outs    <- list(as.name("Y"))
      if (is.null(assigns)) assigns <- list(as.name("Z"))
      return(build_reveal(outs, assigns))
    }
  }
  as.call(lapply(as.list(e), rewrite))
}
`%||%` <- function(x, y) if (is.null(x)) y else x

inquiry_labels <- new.env()

harvest_inquiry <- function(ex) {
  if (is.call(ex) && length(ex) == 3 && as.character(ex[[1]]) %in% c("<-", "=") &&
      is.name(ex[[2]]) && is.call(ex[[3]]) && is.name(ex[[3]][[1]]) &&
      as.character(ex[[3]][[1]]) == "declare_inquiry") {
    a <- as.list(ex[[3]])[-1]; nm <- names(a)
    lab <- if (!is.null(nm) && nzchar(nm[1])) nm[1] else NULL
    if (!is.null(lab)) assign(as.character(ex[[2]]), lab, envir = inquiry_labels)
  }
}

fix_inquiry <- function(e) {
  if (!is.call(e)) return(e)
  a <- as.list(e); nm <- names(a)
  if (!is.null(nm)) for (i in seq_along(a)) {
    if (identical(nm[i], "inquiry") && is.name(a[[i]]) &&
        exists(as.character(a[[i]]), inquiry_labels, inherits = FALSE)) {
      e[[i]] <- get(as.character(a[[i]]), inquiry_labels)
    }
  }
  as.call(lapply(as.list(e), fix_inquiry))
}

res <- list()
for (f in files) {
  nm <- sub("\\.R$", "", basename(f))
  if (!exists(nm, ns)) { res[[nm]] <- "no such export"; next }
  bt <- extract_body(f); if (is.null(bt)) { res[[nm]] <- "no {{{ }}} block"; next }
  env <- new.env(parent = globalenv())
  fm <- formals(get(nm, ns))
  for (a in names(fm)) { if (a %in% c("args_to_fix","...")) next
    v <- tryCatch(eval(fm[[a]], envir = env), error = function(e) NULL)
    if (!is.null(v)) assign(a, v, envir = env) }
  res[[nm]] <- tryCatch({
    exprs <- parse(text = bt)
    rm(list = ls(inquiry_labels), envir = inquiry_labels)
    for (ex in exprs) harvest_inquiry(ex)
    for (ex in exprs) eval(fix_inquiry(rewrite(ex)), envir = env)
    des <- Filter(function(o) inherits(get(o, env), "design"), ls(env))
    if (!length(des)) stop("no design object produced")
    d <- get(des[length(des)], env); set.seed(1)
    paste0("OK  ", nrow(draw_data(d)), " rows, ", length(unclass(d)), " steps")
  }, error = function(e) paste("FAIL:", substr(conditionMessage(e), 1, 78)))
}
ok <- grepl("^OK", unlist(res))
cat(sprintf("\n=== %d of %d run under DeclareDesign 2.0 after AST substitution ===\n\n", sum(ok), length(res)))
for (nm in names(res)) cat(sprintf("  %-38s %s\n", nm, res[[nm]]))

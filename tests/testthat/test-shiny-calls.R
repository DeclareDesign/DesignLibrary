# The app is not exercised by the suite, so a renamed argument it still passes
# by its old name goes unnoticed: after `design` became `.design`, the app's
# `do.call(make_design, c(list(design = id), dots))` sent `design` into `...`
# as a design parameter. These tests read the app's source and check every
# argument it names against the formals of the package function it calls.

package_calls_in <- function(exprs, fns) {
  target_name <- function(f) {
    if (is.call(f) && identical(f[[1]], as.name("::"))) f <- f[[3]]
    if (is.name(f) && as.character(f) %in% fns) as.character(f) else NA_character_
  }
  # Names given inside the `list(...)` or `c(list(...), dots)` that do.call()
  # receives.
  list_arg_names <- function(e) {
    if (!is.call(e)) return(character(0))
    head <- as.character(e[[1]])[1]
    if (head == "list") return(names(as.list(e))[-1])
    if (head == "c") return(unlist(lapply(as.list(e)[-1], list_arg_names)))
    character(0)
  }
  found <- list()
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    fn <- target_name(e[[1]])
    if (!is.na(fn)) {
      found[[length(found) + 1]] <<- list(fn = fn, args = names(as.list(e))[-1])
    }
    if (identical(e[[1]], as.name("do.call")) && length(e) >= 3) {
      fn <- target_name(e[[2]])
      if (!is.na(fn)) {
        found[[length(found) + 1]] <<- list(fn = fn, args = list_arg_names(e[[3]]))
      }
    }
    # An empty argument, as in `x[, 1]`, is the missing symbol and cannot be
    # passed on.
    parts <- Filter(function(p) !identical(p, quote(expr = )), as.list(e))
    for (part in parts) walk(part)
  }
  for (e in exprs) walk(e)
  found
}

# An argument is misnamed when it is not a formal of a function without `...`,
# or when it is a formal's name without the leading dot (`design` for
# `.design`), which a function with `...` would silently take as a parameter.
misnamed_args <- function(calls) {
  bad <- character(0)
  for (cl in calls) {
    formal_names <- names(formals(getExportedValue("DesignLibrary", cl$fn)))
    given <- cl$args[!is.na(cl$args) & nzchar(cl$args %||% "")]
    wrong <- if ("..." %in% formal_names) {
      given[paste0(".", given) %in% formal_names]
    } else {
      setdiff(given, formal_names)
    }
    if (length(wrong)) bad <- c(bad, paste0(cl$fn, "(", wrong, " = )"))
  }
  bad
}

design_fns <- c("make_design", "get_code", "get_args", "design_info", "get_preview")

test_that("the app names only arguments the package functions have", {
  app <- system.file("shiny", "app.R", package = "DesignLibrary")
  skip_if(!nzchar(app), "app.R not found")
  calls <- package_calls_in(parse(app, keep.source = FALSE), design_fns)
  expect_true(any(vapply(calls, function(cl) cl$fn == "make_design", logical(1))))
  expect_identical(misnamed_args(calls), character(0))
})

test_that("the argument check catches the call that broke Run redesign", {
  old <- parse(text = 'design <- do.call(DesignLibrary::make_design, c(list(design = id), st$dots))')
  expect_identical(misnamed_args(package_calls_in(old, design_fns)), "make_design(design = )")
  old_code <- parse(text = 'DesignLibrary::get_code(id, style = "simple")')
  expect_identical(misnamed_args(package_calls_in(old_code, design_fns)), "get_code(style = )")
  fixed <- parse(text = 'do.call(DesignLibrary::make_design, c(list(.design = id), st$dots))')
  expect_identical(misnamed_args(package_calls_in(fixed, design_fns)), character(0))
})

# Register design-id formals so RStudio / Positron / languageserver can
# autocomplete make_design("<tab>") from the installed library.

design_completion_choices <- function() {
  idx <- tryCatch(
    list_designs(shiny_only = FALSE, discover_params = FALSE),
    error = function(e) NULL
  )
  if (is.null(idx) || !nrow(idx)) return("two_arm_simple")
  ids <- as.character(idx$id)
  aliases <- as.character(idx$alias)
  aliases <- aliases[!is.na(aliases) & nzchar(aliases)]
  unique(c(ids, aliases))
}

set_design_arg_choices <- function(fun_name, choices, ns) {
  if (!exists(fun_name, envir = ns, inherits = FALSE)) return(invisible())
  f <- get(fun_name, envir = ns, inherits = FALSE)
  if (!is.function(f) || !"design" %in% names(formals(f))) return(invisible())
  formals(f)$design <- choices
  assign(fun_name, f, envir = ns)
  invisible()
}

.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  choices <- design_completion_choices()
  for (fn in c("make_design", "get_args", "design_info", "get_code", "get_preview")) {
    set_design_arg_choices(fn, choices, ns)
  }
}

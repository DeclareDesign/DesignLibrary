#' Internal helper: emit a defunct error for a DesignLibrary 0.1 export
#'
#' @keywords internal
#' @noRd
.defunct_01 <- function(fn_name, ...) {
  rlang::abort(c(
    paste0("`", fn_name, "()` was removed in DesignLibrary 2.0.0."),
    ...
  ))
}

#' Defunct designer aliases from DesignLibrary 0.1
#'
#' DesignLibrary 0.1 deprecated these aliases in favor of the designers they
#' called. DesignLibrary 2.0.0 removes them. Each errors with a message naming
#' its replacement, which takes the same arguments.
#'
#' | Removed | Use instead |
#' |---|---|
#' | `simple_two_arm_designer()` | [two_arm_designer()] |
#' | `simple_factorial_designer()` | [two_by_two_designer()] |
#' | `simple_iv_designer()` | [binary_iv_designer()] |
#' | `simple_spillover_designer()` | [spillover_designer()] |
#'
#' @param ... (optional) Ignored.
#' @return Never returns; always raises an error.
#' @name simple_two_arm_designer
#' @export
simple_two_arm_designer <- function(...) {
  .defunct_01("simple_two_arm_designer", "i" = "Use `two_arm_designer()`, which takes the same arguments.")
}

#' @rdname simple_two_arm_designer
#' @export
simple_factorial_designer <- function(...) {
  .defunct_01("simple_factorial_designer", "i" = "Use `two_by_two_designer()`, which takes the same arguments.")
}

#' @rdname simple_two_arm_designer
#' @export
simple_iv_designer <- function(...) {
  .defunct_01("simple_iv_designer", "i" = "Use `binary_iv_designer()`, which takes the same arguments.")
}

#' @rdname simple_two_arm_designer
#' @export
simple_spillover_designer <- function(...) {
  .defunct_01("simple_spillover_designer", "i" = "Use `spillover_designer()`, which takes the same arguments.")
}

#' Defunct helpers from DesignLibrary 0.1
#'
#' `get_design_code()` read the `code` attribute that DesignLibrary 0.1
#' designers attached to each design. Designs no longer carry that attribute:
#' each designer builds a library design, and [get_code()] returns that
#' design's source from its id. `match.call.defaults()` was a helper the 0.1
#' designers used to write that code; use [match.call()] and [formals()].
#' Both error with a message saying so.
#'
#' @param ... (optional) Ignored.
#' @return Never returns; always raises an error.
#' @name get_design_code
#' @export
get_design_code <- function(...) {
  .defunct_01(
    "get_design_code",
    "i" = "Designs no longer carry their code as an attribute.",
    "i" = "Use `get_code()` with a library design id, for example `get_code(\"two_arm_flexible\")`.",
    "i" = "`list_designs()` lists the ids, and each designer's help page names the id it builds."
  )
}

#' @rdname get_design_code
#' @export
match.call.defaults <- function(...) {
  .defunct_01(
    "match.call.defaults",
    "i" = "It was an internal helper of the 0.1 designers.",
    "i" = "Use `match.call()`, and `formals()` for the defaults."
  )
}

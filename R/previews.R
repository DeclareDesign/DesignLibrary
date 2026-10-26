#' Load a baked diagnosis preview
#'
#' Reads `inst/previews/<id>.rds` written by [bake_previews()] /
#' [refresh_library()].
#'
#' @param design Design id or book alias.
#' @return A list with at least `id`, `sims`, and `summary`, or `NULL` if missing.
#' @export
get_preview <- function(design) {
  if (length(design) > 1L) design <- design[[1L]]
  key <- normalize_design_key(design)
  parsed <- tryCatch(resolve_design(key), error = function(e) NULL)
  if (is.null(parsed)) return(NULL)

  path <- file.path(previews_dir(), paste0(parsed$meta$id, ".rds"))
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}

#' Whether a baked preview exists for a design
#'
#' @param design Design id or book alias.
#' @return Logical.
#' @export
has_preview <- function(design) {
  !is.null(get_preview(design))
}

#' Character profile for a design (same prose as printing [design_info()])
#'
#' @param design Design id or book alias.
#' @return Character scalar.
#' @export
design_profile <- function(design) {
  paste(utils::capture.output(print(design_info(design))), collapse = "\n")
}

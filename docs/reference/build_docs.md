# Build the pkgdown site (Dropbox-safe)

On Windows Dropbox folders,
[`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html)
/ `build_article()` often fail in
[`xml2::write_html()`](http://xml2.r-lib.org/reference/write_xml.md)
with "Invalid argument" / "Error closing file" when writing directly
into `docs/`. This helper builds into a local temp directory, then
copies the result into `docs/`.

## Usage

``` r
build_docs(pkg = NULL, ...)
```

## Arguments

- pkg:

  Package root. Default: `find_package_root()` via
  `options(DesignLibrary.root=...)` or the current working directory.

- ...:

  Passed to
  [`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html)
  (for example `devel = TRUE`).

## Value

Invisibly, the path to `docs/`.

## Examples

``` r
if (FALSE) { # \dontrun{
options(DesignLibrary.root = "C:/path/to/DesignLibrary")
build_docs()
} # }
```

# Copy the bundled Shiny app to a standalone folder

Materializes `app.R` (and `www/` if present) so a Shiny Server / Posit
Connect host can point at a folder. The folder relies on the installed
`DesignLibrary` package for designs and helpers.

## Usage

``` r
copy_library_shiny(dest = getwd(), overwrite = TRUE, package = "DesignLibrary")
```

## Arguments

- dest:

  Directory to write (created if needed). Default: working directory.

- overwrite:

  If `TRUE`, replace `app.R` / `www` files (not `local.R`).

- package:

  Package that ships the app; default `"DesignLibrary"`.

## Value

Invisibly, the normalized destination path.

## Details

Typical server workflow:

    remotes::install_github("DeclareDesign/DesignLibrary@RDrewrite")
    DesignLibrary::install_library_dependencies()
    DesignLibrary::copy_library_shiny("/srv/shiny-server/designlibrary")

Existing `local.R` in `dest` is never overwritten.

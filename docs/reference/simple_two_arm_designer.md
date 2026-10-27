# Defunct designer aliases from DesignLibrary 0.1

DesignLibrary 0.1 deprecated these aliases in favor of the designers
they called. DesignLibrary 2.0.0 removes them. Each errors with a
message naming its replacement, which takes the same arguments.

## Usage

``` r
simple_two_arm_designer(...)

simple_factorial_designer(...)

simple_iv_designer(...)

simple_spillover_designer(...)
```

## Arguments

- ...:

  (optional) Ignored.

## Value

Never returns; always raises an error.

## Details

|  |  |
|----|----|
| Removed | Use instead |
| `simple_two_arm_designer()` | [`two_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/two_arm_designer.md) |
| `simple_factorial_designer()` | [`two_by_two_designer()`](https://declaredesign.org/r/designlibrary/reference/two_by_two_designer.md) |
| `simple_iv_designer()` | [`binary_iv_designer()`](https://declaredesign.org/r/designlibrary/reference/binary_iv_designer.md) |
| `simple_spillover_designer()` | [`spillover_designer()`](https://declaredesign.org/r/designlibrary/reference/spillover_designer.md) |

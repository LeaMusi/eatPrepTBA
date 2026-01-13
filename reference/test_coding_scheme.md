# Test Coding Scheme

This function can be used to determine frequently made mistakes in the
coding scheme.

## Usage

``` r
test_coding_scheme(
  data,
  exceptions = list(),
  name_list = FALSE,
  console = FALSE
)
```

## Arguments

- data:

  IQB-Studio data. Needs to be prepared with 'get_units()',
  'get_coding_report()' and 'add_coding_scheme()'.

- exceptions:

  A list of unit-exceptions from the tests. Needs to be given as
  'exceptions = list("test_nr" = "unit_key")', for example 'exceptions =
  list("8" = "MZB270")'.

- name_list:

  A logical vector of TRUE or FALSE to identify testnumbers for
  'exceptions'. If TRUE, only testname and testnumber are printed.
  Default is FALSE.

- console:

  A logical vector of TRUE or FALSE. If TRUE, testthat() output is
  printed to console in addition to the html-table. Default is FALSE.

## Value

An html-table and console output if needed.

## Examples

``` r
if (FALSE) { # \dontrun{
test_coding_scheme(
  data = data, 
  name_list = FALSE, 
  console = FALSE, 
  exceptions = rlang::list2("8" = "MZB270","12" = "MMB035")
)
} # }
```

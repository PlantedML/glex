# Bikesharing data

A reduced version of the `Bikeshare` data as included with `ISLR2`. The
dataset has been converted to a
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html),
with the following changes:

## Usage

``` r
bike
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 8645
rows and 11 columns.

## Source

`Bikeshare` in package `ISLR2`

## Details

- `hr` has been copnverted to a numeric

- `workingday` was recoded to a binary `factor` with labels
  `c("No Workingday", "Workingday")`

- `season` was recoded to a `factor` with labels
  `c("Winter", "Spring", "Summer", "Fall")`

- Variables `atemp`, `day`, `registered` and `casual` were removed

# Validate names of input data columns

Validate names of input data columns

## Usage

``` r
validate_column_names(expected, actual, dataset)
```

## Arguments

- expected:

  Character vector of expected column names

- actual:

  Character vector of actual column names

- dataset:

  Name of dataset (used in outputted message only)

## Value

A string describing the issue found, or NULL if no issues is found.

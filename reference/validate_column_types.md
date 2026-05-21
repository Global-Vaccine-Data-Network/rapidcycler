# Validate names of input data columns

Validate names of input data columns

## Usage

``` r
validate_column_types(data, expected, data_name)
```

## Arguments

- data:

  The dataset being checked

- expected:

  Named character vector of expected column types, in the form
  "COLUMN_NAME" = "COLUMN_TYPE"

- data_name:

  Name of dataset (used in outputted message only)

## Value

A character vector of issues found. If no issues are found, the vector
will be empty.

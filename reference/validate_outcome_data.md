# Validate AESI outcome data

Validate AESI outcome data

## Usage

``` r
validate_outcome_data(outcome_data, person_data, options, split_size)
```

## Arguments

- outcome_data:

  AESI outcome data input data.table

- person_data:

  Person data input data.table

- options:

  Options object, already converted from txt file. Supply either
  options_file_location or options, not both

- split_size:

  Input data will be automatically split into pieces of this size
  (default 500,000). If memory is an issue, lower the split_size.

## Value

A character vector of issues found. If no issues are found, the vector
will be empty.

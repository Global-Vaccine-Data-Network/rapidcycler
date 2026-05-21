# Validate outcome dates

Validate outcome dates

## Usage

``` r
validate_outcome_dates(person_data, outcome_data, split_size)
```

## Arguments

- person_data:

  Person data input data.table

- outcome_data:

  AESI outcome data input data.table

- split_size:

  Input data will be automatically split into pieces of this size
  (default 500,000). If memory is an issue, lower the split_size.

## Value

A character vector of issues found. If no issues are found, the vector
will be empty.

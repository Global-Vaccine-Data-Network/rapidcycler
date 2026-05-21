# Validate vaccination dates

Validate vaccination dates

## Usage

``` r
validate_vaccination_dates(person_data, vaccination_data, split_size)
```

## Arguments

- person_data:

  Person data input data.table

- vaccination_data:

  Vaccination data input data.table. Set to NULL to perform historical
  data aggregation

- split_size:

  Input data will be automatically split into pieces of this size
  (default 500,000). If memory is an issue, lower the split_size.

## Value

A character vector of issues found. If no issues are found, the vector
will be empty.
